#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2);library(dplyr);library(tidyr);library(data.table);library(leaflet);library(leaflet.extras)
library(lubridate);library(magrittr);library(stringr);library(purrr);library(shiny);library(rvest)

myIcons <- iconList("algemeen" = makeIcon("green.png", iconWidth = 18, iconHeight = 24), 
                    "vrij algemeen" = makeIcon("blue.png", iconWidth = 18, iconHeight = 24),
                    "zeldzaam" = makeIcon("amber.png", iconWidth = 18, iconHeight =24),
                    "zeer zeldzaam" = makeIcon("red.png", iconWidth = 18, iconHeight =24))


#### Define UI for application that draws a map plotting bird observations ####
ui <- fluidPage(
    title="Observations Map",
    tags$head(tags$script(src = "message-handler.js"),
              tags$link(rel="shortcut icon", href="parroticon.png")),
    leafletOutput("mymap", width = "100%", height = 800),
    absolutePanel(top = 10, left = 100, 
                  wellPanel(
                      sliderInput("time", "Date of observation", 
                                  floor_date(today()), 
                                  ceiling_date(Sys.time(),"hours"),
                                  value = c(floor_date(today()),ceiling_date(Sys.time(),"hours")),
                                  step=3600),
                      textInput("userid", "Waarneming.nl User ID", value = "130065"),
                      radioButtons("lifer","",
                                   choices = list("Only show LIFERs" = 1, "Show all observations" = 2), 
                                   selected = 1)
                      ), style = "opacity: 0.92" # https://shiny.rstudio.com/gallery/absolutely-positioned-panels.html
    )
)

#### Define server logic required to map bird observations ####
server <- function(input, output, session) {
    session$onSessionEnded(stopApp)

    lifelist <- reactive({
        tryCatch(
            {lifelist.page <- read_html(paste(sep="","https://waarneming.nl/users/",input$userid,"/species/?species_group=1&start_date=&end_date=&province=0&use_local_taxonomy=on&include_exotic_and_extinct=on&include_escapes=on"))%>%
                html_node("table")%>%
                html_table()
            lifelist.page$naam}, 
            error = function(e) {
                0
            }
        )
    })
    
    provdata <- reactive({
        readRDS(paste(sep="","LIFER_obs_",0,"_2019-06-28.rds"))
    })
    
    liferdata <- reactive({
        if (input$lifer == 1){
           provdata()[!provdata()$species %in% lifelist(), ] # https://stackoverflow.com/questions/15227887
        } else {
           provdata()
        }
    })
    
    filteredData <- reactive({
        from<- input$time[1]
        till<- input$time[2]
        liferdata() %>% filter(date >= from & date <=  till)
    })
    
    labelcontent <- reactive({
        paste(sep="",
              "<b>",filteredData()$species,"</b>&emsp;[",filteredData()$rarity,"]",
              "<br/>Seen on ",filteredData()$date,
              "<br/>Location: ",filteredData()$location,
              "<br/><a href=",filteredData()$link," target='_blank'>",filteredData()$link,"</a>")
    })
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addProviderTiles("OpenStreetMap.HOT", group = "OSM") %>%
            addProviderTiles("Esri.WorldImagery",group = "Satellite")%>%
            fitBounds(lng1 = min(provdata()$lon),lat1 = min(provdata()$lat),
                      lng2 = max(provdata()$lon),lat2 = max(provdata()$lat))%>%
            addControlGPS(options = gpsOptions(position = "topleft", activate = FALSE, 
                                               autoCenter = TRUE, maxZoom = 11, 
                                               setView = FALSE))
    })
    
    observe({
        leafletProxy("mymap", data = filteredData()) %>%
            clearMarkers() %>%
            addMarkers(icon = ~myIcons[rarity],~lon, ~lat, popup = labelcontent(), group = ~rarity)%>%
            addLayersControl(baseGroups = c("OSM", "Satellite"),
                             options = layersControlOptions(collapsed = FALSE),
                             overlayGroups = ~rarity)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
