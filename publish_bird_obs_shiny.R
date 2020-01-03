#### install and load the required packages ####
for (package in c('readr','broom', 'tidyverse', 'lubridate', 'ggpubr', 'zoo', 'data.table', 'shiny', 'rsconnect', 'Hmisc',
                  'rvest', 'tcltk', 'packrat', 'plogr','BH', 'svDialogs')) {
  if (!require(package, character.only = T)) {
    print(paste('installing:', package))
    install.packages(package) }
  library(package, character.only = T)
}

#### Connect to shiniapps.io using your personal secret ####
{
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  secret <- dlg_input("Enter your shinyapps.io secret:","")$res
  if (!length(secret)) {# The user clicked the 'cancel' button
    cat("Please enter a secret\n")
  } else {
    rsconnect::setAccountInfo(name = 'fredderks', token = '0698F57CA0922627ECA5EED201D748BF', secret = secret)
  }
}

#### Scrape data for all LIFER species observed today ####
{
  source("scrape_lifer_obs.R")
  sink("log.txt", append = TRUE, split = TRUE)
  cat("\n", as.character(Sys.time()), "\n")
  # file <- paste(sep="", "LIFER_obs_0_",today()-1,".rds") # Retrieve yesterday's file
  file <- list.files(pattern = ".rds") %>% max() # Retrieve last file produced
  ScrapeLiferObs(0,1,file)
  date <- list.files(pattern = ".rds") %>% max() %>% gsub("(LIFER_obs_._)|(.rds)","",.)
  deployApp(appDir = getwd(),appFiles = c(paste(sep = "","LIFER_obs_0_",date,".rds"),"www","styles.css","app.R"), forceUpdate = TRUE)
  sink()
}