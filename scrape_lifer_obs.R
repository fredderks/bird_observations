library(magrittr);library(lubridate);library(zoo);library(data.table);library(rsconnect);library(rvest)
library(Hmisc);library(tcltk);library(selectr);library(tibble);library(dplyr);library(stringr)
ScrapeLiferObs <- function(province,rarity,oldfile) {
  
  commonlist <- read.csv("commonlist.csv", header = T) # List of common species
  date <- today()
  uncertain <- 0
  nogps <- 0
  exclusions <- c(" ssp ", " x ", " of ","(verwilderd)"," onbekend") %>% paste(collapse = "|")
  df <- data.frame("date" = as.POSIXct(character()),
                   "species" = character(),
                   "rarity" = character(),
                   "location" = character(), 
                   "number" = character(), 
                   "validity" = character(),
                   "link" = character(),
                   "lat" = numeric(), 
                   "lon" = numeric(),
                   stringsAsFactors = FALSE)

# Gathering URLs for new observations -------------------------------------
  
  ## Species Pages
  rootpage.html <- read_html(paste("https://waarneming.nl/fieldwork/observations/daylist/?date=",date,"&species_group=1&province=",province,"&rarity=",rarity,"&search=", sep = ""))
  species.pages <- rootpage.html %>%  # On Root, Gather links to species specific pages if there is more than one observation of said species
    html_nodes("table") %>% #  http://flukeout.github.io/ # https://www.w3schools.com/css/css_attribute_selectors.asp 
    html_nodes("a[href*='/species/']")
  
  species.pages <- tibble( name = 
                             species.pages %>%
                             html_text() %>%
                             sub(' (.*) -.*','\\1',.), 
                           url =
                             species.pages %>%
                             html_attr("href")) # Retrieve names and urls from all species observed today
  
  spec.pages.uncommon <- species.pages %>% filter( 
    !grepl(exclusions, name), # Only keep species that are not excluded 
    !name %in% commonlist$naam # Only keep species that are not common species
  )
  
  ## Individual Observations Pages
  obs.pages <- rootpage.html %>% # On Root page, Gather links to observation pages if they are linked directly, i.e. there is only one observation
    html_nodes("table") %>% 
    html_nodes("a[href*='/observation/']")
  
  obs.pages <- tibble( name = 
                         obs.pages %>%
                         html_text() %>%
                         sub(' (.*) -.*','\\1',.), 
                       url =
                         obs.pages %>%
                         html_attr("href")) # Retrieve names and urls from all observations made today
  
  obs.pages.uncommon <- obs.pages %>% filter( 
    !grepl(exclusions, name), # Only keep observations that are not excluded 
    !name %in% commonlist$naam # Only keep bservations that are not common species
  )
  obs.links.uncommon <- obs.pages.uncommon$url
  
  ## On Species pages, gather links to individual observation pages
  for (link in spec.pages.uncommon$url) { # Open every species page 
    species.html <- read_html(paste("https://waarneming.nl",link,sep = ""))  
    
    species.obs.links <- species.html %>% # Retrieve unique URLs
      html_nodes("table") %>% 
      html_nodes("a[href*='/observation/']") %>%
      html_attr("href")
    
    obs.links.uncommon %<>%
      append(species.obs.links, after = length(obs.links.uncommon)) # Append to existing observation URLs
  }
  
  obs.links.old <- tryCatch( # Find all urls already retrieved when this script last ran
    {gsub("https://waarneming.nl", '', readRDS(oldfile)[,7])},
    error = function(e) { NULL }
  )

  obs.links.uncommon <- setdiff(obs.links.uncommon,obs.links.old) # Finds non-overlapping elements that are contained in the first object
  
  print(paste(length(obs.links.uncommon),"new observations found on date:",as.Date(date)))
  
# Web scraping location details from all new observations -----------------

  # Progress bar
  total <- length(obs.links.uncommon); ind <- 0
  pb <- tkProgressBar(title = "progress bar", min = 0,
                      max = total, width = 300)
  
  for (link in obs.links.uncommon) { # On all observation pages, gather the required info.
    obs.url <- paste("https://waarneming.nl",link,sep = "")
    obs.html <- read_html(obs.url)
    
    if (length(obs.html %>% html_nodes("i[class*='fas fa-question-square status-uncertain fa-fw']")) == 0 ) { # Test wether observation is certain
      
      obs.gps <-
        obs.html %>%
        html_node("span.teramap-coordinates-coords") %>%
        html_text()
      
      if (!is.na(obs.gps)) { # only continue if gps coordinates are present
        
        obs.details <- # Gather all the details from the observation page
          obs.html %>%
          html_node("table.table") %>%
          html_table() %>%
          data.frame(row.names = .[,1]) %>%
          select(-X1) %>%
          t() %>%
          data.frame(stringsAsFactors = F) %>%
          mutate(
            lat = str_split_fixed(string = obs.gps, pattern = ", ", n = 2)[1] %>% as.numeric(),
            lon = str_split_fixed(string = obs.gps, pattern = ", ", n = 2)[2] %>% as.numeric(),
            name = obs.html %>% html_node("span.species-common-name") %>% html_text(),
            rarity =  obs.html %>% html_node("span.hidden-xs") %>% html_text(),
            validity =   obs.html %>% html_node("td.validation-status-text") %>% html_text(),
            url = obs.url
          )
        
        if (grepl("goedgekeurd", obs.details$validity)) { # Rename validity markers
          obs.details$validity <- "confirmed"
        } else if (grepl("onbekend", obs.details$validity)) {
          obs.details$validity <- "unknown"
        } else {
          obs.details$validity <- "other"
        }
        
        df[nrow(df) + 1,] <- obs.details %>% select(datum, name, rarity, locatie, aantal ,validity, url, lat, lon)
      } else {nogps <- nogps + 1} # Count number of obs skipped because they are without gps coordinates (embargo)
    } else {uncertain <- uncertain + 1} # Count number of obs skipped because they are uncertain
    
    ind <- ind + 1
    setTkProgressBar(pb, ind, label = paste( round(ind / total * 100, 0), "% done"))
  } # End of observation page
  
  df %<>% setorder(date)
  
  filecomp <- tryCatch(
    {
      file.old <- readRDS(oldfile)
      rbind(file.old,df)
    }, 
    error = function(e) {
      df
    }
  )
  
  saveRDS(filecomp,paste("LIFER_obs_",province,"_",today(),".rds",sep = ""))
  print(paste("Missing values:", uncertain, "uncertain,", nogps,"without gps coordinates."))
  print(paste(length(unique(df$species)),"new uncommon species were observed today."))
  close(pb)
}