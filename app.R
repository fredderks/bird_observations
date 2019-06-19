#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2);library(dplyr);library(tidyr);library(data.table);library(leaflet)
library(lubridate);library(magrittr);library(stringr);library(purrr);library(shiny)

GPS_obs <- readRDS("obs_gps_309_2019-05-19_2019-06-18.rds")

#### Define UI for application that draws a map plotting bird observations ####
ui <- fluidPage(
    leafletOutput("mymap", width = "100%", height = 800),
    absolutePanel(top = 10, left = 100,
                  sliderInput("time", "Date of observation", 
                              floor_date(min(GPS_obs$datum),"hours"), 
                              ceiling_date(max(GPS_obs$datum),"hours"),
                              value = floor_date(min(GPS_obs$datum),"hours"),
                              step=3600,
                              animate = animationOptions(interval = 1))
    )
)

#### Define server logic required to map bird observations ####
server <- function(input, output, session) {
    session$onSessionEnded(stopApp)
    
    filteredData <- reactive({
        from<- min(GPS_obs$datum)
        till<- input$time
        GPS_obs %>% filter(datum >= from & datum <=  till)
    })
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addProviderTiles("OpenStreetMap.HOT") %>%
            fitBounds(lng1 = min(GPS_obs$lon),lat1 = min(GPS_obs$lat),
                      lng2 = max(GPS_obs$lon),lat2 = max(GPS_obs$lat))
    })
    observe({
        leafletProxy("mymap", data = filteredData()) %>%
            clearMarkers() %>%
            addMarkers(~lon, ~lat, label = ~paste("Pied Crow, Date:", (GPS_obs$datum)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
