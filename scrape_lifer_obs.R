library(ggplot2);library(dplyr);library(ggpubr);library(zoo);library(tidyr);library(data.table);library(shiny);library(leaflet);library(lubridate);library(magrittr);library(stringr);library(purrr);library(rsconnect);library(rvest);library(Hmisc);library(tcltk)

ScrapeLiferObs <- function(userid,province,rarity,oldfile) {
  date <- today()
  uncertain <- 0
  nogps <- 0
  exclusions <- c(" ssp ", " x ", " of ","(verwilderd)"," onbekend")
  df <- data.frame("date" = as.POSIXct(character()),
                   "species" = character(),
                   "rarity" = character(),
                   "location" = character(), 
                   "number" = character(), 
                   "validity" = character(),
                   "link" = character(),
                   "lat" = character(), 
                   "lon" = character(),
                   stringsAsFactors=FALSE)
  
  lifelist.link <- paste(sep="","https://waarneming.nl/users/",userid,"/species/?species_group=1&start_date=&end_date=&province=0&use_local_taxonomy=on&include_exotic_and_extinct=on&include_escapes=on") 
  
  lifelist.html <- read_html(lifelist.link)%>%
    html_nodes("table")
  
  lifelist.names <- html_table(lifelist.html[[1]]) 
  lifelist.names %<>% 
    rbind(html_table(lifelist.html[[2]]))%>%
    rbind(html_table(lifelist.html[[3]]))%>%
    rbind(html_table(lifelist.html[[4]])) # Bind tables for species as well as subspecies and formas
  lifelist.names <- lifelist.names[c(2,3)]
  
  lifelist.num <- lifelist.html[1:4]%>%
    html_nodes("td[data-sort-value]")%>%
    html_nodes("a[href*='/observations/?species=']")%>%
    html_attr("href")
  lifelist.names$num <- str_match(lifelist.num, "species=(.*?)&after_date")[,2] # Add matching species id numbers
  
  if (1413 %nin% lifelist.names[,3]){
    lifelist.names %<>% rbind(c("Parkgans","Anser anser forma domestica",1413))
  }  
  if (1412 %nin% lifelist.names[,3]){
    lifelist.names %<>% rbind(c("Parkeend","Anas platyrhynchos forma domestica",1412))
  }  
  if (80199 %nin% lifelist.names[,3]){
    lifelist.names %<>% rbind(c("Stadsduif","Columba livia forma domestica",80199))
  }
  if (18926 %nin% lifelist.names[,3]){
    lifelist.names %<>% rbind(c("Vogel onbekend","Aves indet.",18926))
  }
  rootpage.html <- read_html(paste("https://waarneming.nl/fieldwork/observations/daylist/?date=",date,"&species_group=1&province=",province,"&rarity=",rarity,"&search=", sep=""))
  species.links <- rootpage.html %>%  # On Root, Gather links to species specific pages of there is more than one observation of said species
    html_nodes("table") %>% #  http://flukeout.github.io/ # https://www.w3schools.com/css/css_attribute_selectors.asp 
    html_nodes("a[href*='/species/']")
  
  species.names <- species.links%>%
    html_text() %>%
    str_match(. ," (.*?) -") %>% .[,2]
  species.links <- species.links %>%
    html_attr("href")
  
  species.excl <- grep(paste(exclusions,collapse="|"), 
                       species.names, value=TRUE, invert = TRUE)
  species.links <- subset(species.links,species.names %in% species.excl)
  spec.links.lifer <- subset(species.links,species.excl %nin% lifelist.names$naam)# Only scrape links that are lifer species
  
  # species.links <- as.data.frame(subset(species.links, grepl("observation", species.links)))
  # species.links$num <- str_match(species.links[,1], "species/(.*?)/observations")[,2]
  # spec.links.lifer <- as.vector(subset(species.links[,1],species.links[,2] %nin% lifelist.names$num)) 
  
  obs.links <- rootpage.html %>% # On Root page, Gather links to observation pages if they are linked directly, i.e. there is only one observation
    html_nodes("table") %>% 
    html_nodes("a[href*='/observation/']")
  
  common.names <- obs.links %>%
    html_text() %>%
    str_match(. ," (.*?) -") %>% .[,2]
  root.links <- obs.links %>%
    html_attr("href")
  
  common.excl <- grep(paste(exclusions,collapse="|"), 
                      common.names, value=TRUE, invert = TRUE)
  
  root.links <- subset(root.links,common.names %in% common.excl)
  
  obs.links.lifer <- subset(root.links,common.excl %nin% lifelist.names$naam) # Only scrape links that are lifer species
  
  for (link in spec.links.lifer){ # On Species pages, gather links to individual observation pages
    species.html <- read_html(paste("https://waarneming.nl",link,sep=""))
    
    species.obs.links <- species.html %>%
      html_nodes("table") %>% 
      html_nodes("a[href*='/observation/']") %>%
      html_attr("href")
    obs.links.lifer %<>%
      append(species.obs.links, after = length(obs.links.lifer))
  }
  
  obs.links.old <- tryCatch(
    {
      obs.links.old <- readRDS(oldfile)[,7]
      gsub("https://waarneming.nl", '',obs.links.old)
    }, 
    error = function(e) {
      NULL
    }
  )
  
  obs.links.lifer <- setdiff(obs.links.lifer,obs.links.old) # finds non-overlapping elements that are contained in the first object
  
  print(paste(length(obs.links.lifer),"new LIFER observations found on date:",as.Date(date)))
  
  total <- length(obs.links.lifer); ind <- 0
  pb <- tkProgressBar(title = "progress bar", min = 0,
                      max = total, width = 300)
  
  for (link in obs.links.lifer){ # On all observation pages, gather the required info.
    obs.link <- paste("https://waarneming.nl",link,sep="")
    obs.html <- read_html(obs.link)
    
    if (length(obs.html %>% # Test wether observation is certain
               html_nodes("i[class*='fas fa-question-square status-uncertain fa-fw']")) == 0 ){
      
      obs.details <-
        obs.html %>%
        html_node("table.table") %>%
        html_table() %>%
        t() 
      colnames(obs.details) <- obs.details[1,]
      obs.details <- obs.details[-1,]
      obs.details %<>%
        t()%>%
        data.frame()
      
      obs.date <- as.character(obs.details$datum)
      obs.location <- as.character(obs.details$locatie)
      obs.number <- obs.details$aantal %>%
        as.character %>%
        gsub('[[:space:]]', ' ', .)
      
      obs.gps <-
        obs.html %>%
        html_node("span.teramap-coordinates-coords")%>%
        html_text()
      if (!is.na(obs.gps)){ # only continue if gps coordinates are present
        obs.gps %<>%
          str_split_fixed(", ",2)
        obs.name <-
          obs.html %>%
          html_node("span.species-common-name")%>%
          html_text()
        obs.rarity <-
          obs.html %>%
          html_node("span.hidden-xs")%>%
          html_text()
        obs.validity <-
          obs.html %>%
          html_node("td.validation-status-text")%>%
          html_text() 
        
        if (grepl("goedgekeurd", obs.validity)){
          obs.validity <- "confirmed"
        } else if(grepl("onbekend", obs.validity)){
          obs.validity <- "unknown"
        } else {
          obs.validity <- "other"
        }
        df[nrow(df)+1,] <- c(obs.date,obs.name,obs.rarity,obs.location,obs.number,obs.validity,obs.link,obs.gps[1],obs.gps[2])
      } else {nogps <- nogps + 1}
    } else {uncertain <- uncertain + 1}
    ind <- ind + 1
    setTkProgressBar(pb, ind, label=paste( round(ind/total*100, 0),
                                           "% done"))
  } # End of observation page
  df %<>% setorder(date)
  df$lat %<>% as.numeric()
  df$lon %<>% as.numeric()
  
  
  filecomp <- tryCatch(
    {
      file.old <- readRDS(oldfile)
      rbind(file.old,df)
    }, 
    error = function(e) {
      df
    }
  )
  
  saveRDS(filecomp,paste("LIFER_obs_",province,"_",today(),".rds",sep=""))
  print(paste("Missing values:", uncertain, "uncertain,", nogps,"without gps coordinates."))
  print(paste(length(unique(df$species)),"new LIFER species were observed today."))
  close(pb)
}