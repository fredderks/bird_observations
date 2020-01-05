#### install and load the required packages ####
for (package in c('readr','broom', 'tidyverse', 'lubridate', 'ggpubr', 'zoo', 'data.table', 'shiny', 'rsconnect', 'Hmisc',
                  'rvest', 'tcltk', 'packrat', 'plogr','BH', 'svDialogs')) {
  if (!require(package, character.only = T)) {
    print(paste('installing:', package))
    install.packages(package) }
  library(package, character.only = T)
}

#### Required functions ####
ConnectShinyapps <- function() {
  correct <- 0
  correct <- tryCatch({
    account <- rsconnect::accountInfo('fredderks')[[1]]
    cat("\nLogged in using account:", account, "\n")
    correct <- 3
  }, error = function(e) {
    cat("\nNo account found, please log in\n")
    while (TRUE) {
      setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
      if (correct == 1) {
        secret <- dlg_input("Sorry, that secret was too short\nPlease enter your shinyapps.io secret:","")$res
      } else if (correct == 2) {
        secret <- dlg_input("Sorry, that secret was incorrect\nPlease enter your shinyapps.io secret:","")$res
      } else if (correct == 3) {
        dlg_message("The user has already successfully logged in")
        break
      } else {
        secret <- dlg_input("Enter your shinyapps.io secret:","")$res
      }
      if (!length(secret)) {# The user clicked the 'cancel' button
        cat("The user has canceled\n")
        correct <- 4
        break
      } else if (nchar(secret) < 40 ){
        correct <- 1
      } else {
        correct <- tryCatch({
          rsconnect::setAccountInfo(name = 'fredderks', token = '0698F57CA0922627ECA5EED201D748BF', secret = secret)
          cat("Successfully logged in\n")
          correct <- 3
          break
        }, error = function(e) {
          message(e,"")
          Sys.sleep(0.5)
          correct <- 2
          return(2)
        })
      }
    }
    return(correct)
  })
  if (correct == 3) return(TRUE) else return(FALSE)
}

PublishObservations <- function() {
  connection <- FALSE
  source("scrape_lifer_obs.R")
  sink("log.txt", append = TRUE, split = TRUE)
  connection <- ConnectShinyapps()
  if (connection) {
    cat(as.character(Sys.time()), "\n")
    file <- list.files(pattern = ".rds") %>% max() # Retrieve last file produced so it can potentially be added to the new file
    tryCatch ({
      ScrapeLiferObs(0,1,file)
      while(TRUE){
        if (connection) {
          deployApp(appDir = getwd(),appFiles = c(paste(list.files(pattern = ".rds") %>% max()),"www","styles.css","app.R"), 
                    appName = "bird_observations_master", forceUpdate = TRUE)
          break
        } else {
          res <- dlg_message("Please make a connection to the server first", "okcancel")$res
          if (res == "cancel") break else connection <- ConnectShinyapps()
        }
      }
    }, error = function(e) {
      message(e,"")
      Sys.sleep(0.5)
    })
  } 
  for(i in seq_len(sink.number())){
    sink(NULL)}
  closeAllConnections()
}

#### Scrape data for all LIFER species observed today ####

PublishObservations()
