#
# This Shiny Web Applicaton takes data gathered from waarnemingen.nl and plots bird obserations
# on a map. The slider can be used to reveal the observations chronologically
#
#

library(ggplot2);library(dplyr);library(tidyr);library(data.table);library(leaflet)
library(lubridate);library(magrittr);library(stringr);library(purrr);library(shiny);library(rsconnect)

GPS_obs <- readRDS("obs_gps_309_2019-05-19_2019-06-18.rds")

#### Define UI for application that draws a map plotting bird observations ####
ui <- fluidPage(
    leafletOutput("mymap", width = "100%", height = 800), 
    
    absolutePanel(top = 10, left = 100,
                  
                  sliderInput("time", "Date of observation", 
                              floor_date(min(GPS_obs$datum),"hours"), 
                              ceiling_date(max(GPS_obs$datum),"hours"),
                              value = floor_date(min(GPS_obs$datum),"hours"),
                              step=3600)#, animate = animationOptions(interval = 1) # Currently takes about 18s to animate all markers
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
