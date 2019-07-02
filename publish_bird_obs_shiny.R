rm(list=ls())
library(ggplot2);library(dplyr);library(ggpubr);library(zoo);library(tidyr);library(data.table);library(shiny);library(leaflet);library(lubridate);library(magrittr);library(stringr);library(purrr);library(rsconnect);library(rvest);library(Hmisc);library(tcltk);library(rsconnect)
setwd("E:/Google Drive/Coding/GitHub/bird_observations")
#### Scrape data for all LIFER species observed in a certain province today ####
# userid <- 130065; province <- 0; rarity <- 1 ; oldfile <- "LIFER_obs_0_2019-06-27.rds"

source("scrape_lifer_obs.R")
sink("log.txt", append = TRUE, split = TRUE)
Sys.time()
file <- paste(sep="", "LIFER_obs_0_",today()-1,".rds")
ScrapeLiferObs(130065,0,1,file)
deployApp(appDir = getwd(),appFiles = c(paste(sep="","LIFER_obs_0_",today(),".rds"),"www","app.R"), forceUpdate = TRUE)
sink()
  