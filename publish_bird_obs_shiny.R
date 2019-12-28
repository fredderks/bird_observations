library(tidyverse);library(lubridate);library(ggpubr);library(zoo);library(data.table);library(shiny)
library(rsconnect);library(Hmisc);library(rvest);library(tcltk);library(packrat);library(plogr);library(BH) #TODO install BH
# setwd("E:/Google Drive/Coding/GitHub/bird_observations")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#### Scrape data for all LIFER species observed in a certain province today ####

source("scrape_lifer_obs.R")
sink("log.txt", append = TRUE, split = TRUE)
cat("\n", as.character(Sys.time()), "\n")
# file <- paste(sep="", "LIFER_obs_0_",today()-1,".rds") # Retrieve yesterday's file
file <- list.files(pattern = ".rds") %>% max() # Retrieve last file produced
ScrapeLiferObs(0,1,file)
date <- list.files(pattern = ".rds") %>% max() %>% gsub("(LIFER_obs_._)|(.rds)","",.)
deployApp(appDir = getwd(),appFiles = c(paste(sep = "","LIFER_obs_0_",date,".rds"),"www","styles.css","app.R"), forceUpdate = TRUE)
sink()
