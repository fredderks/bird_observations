library(tidyverse);library(ggpubr);library(zoo);library(data.table);library(shiny);library(rsconnect);library(rvest);library(Hmisc);library(tcltk)
setwd("E:/Google Drive/Coding/GitHub/bird_observations")
#### Scrape data for all LIFER species observed in a certain province today ####

source("scrape_lifer_obs.R")
sink("log.txt", append = TRUE, split = TRUE)
print("")
print(Sys.time())
# file <- paste(sep="", "LIFER_obs_0_",today()-1,".rds") # Retrieve yesterday's file
file <- list.files(pattern = ".rds") %>% max() # Retrieve last file produced
ScrapeLiferObs(0,1,file)
deployApp(appDir = getwd(),appFiles = c(paste(sep="","LIFER_obs_0_",today(),".rds"),"www","styles.css","app.R"), forceUpdate = TRUE)
sink()