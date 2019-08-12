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

provdata <- readRDS(paste(sep="","LIFER_obs_",0,"_",today(),".rds"))
provdata$province <- sub(".* ", "", provdata$location)

provinces <- c("All Provinces"="", "Drenthe"="(DR)", "Flevoland"="(FL)", "Friesland"="(FR)",
               "Gelderland"="(GE)", "Groningen"="(GR)", "Limburg"="(LI)", "Noord-Brabant"="(NB)",
               "Noord-Holland"="(NH)", "Overijssel"="(OV)", "Zuid-Holland"="(ZH)", "Utrecht"="(UT)",
               "Zeeland"="(ZL)")
#### Define UI for application that draws a map plotting bird observations ####
ui <- navbarPage("Observations Map",
    tabPanel("Interactive Observations Map",
        div(class="outer",
            tags$head(
                # Include our custom CSS
                includeCSS("styles.css"),
                tags$script(src = "message-handler.js"),
                tags$link(rel="shortcut icon", href="parroticon.png")
                ),
        leafletOutput("mymap", width = "100%", height = "100%"),
        absolutePanel(top = 20, left = 70, width = 300,
                      wellPanel(
                          sliderInput("time", "Date of observation", 
                                      floor_date(today()-7), 
                                      ceiling_date(Sys.time(),"hours"),
                                      value = c(floor_date(today()),ceiling_date(Sys.time(),"hours")),
                                      step=3600, ticks = F),
                          textInput("userid", "Waarneming.nl User ID", value = "130065", width = 140),
                          radioButtons("lifer","",
                                       choices = list("Only show LIFERs" = 1, "Show all observations" = 2), 
                                       selected = 1)
                          ), 
                      style = "opacity: 0.92" # https://shiny.rstudio.com/gallery/absolutely-positioned-panels.html
            )
        )
    ),
    tabPanel("Data Explorer",
             fluidRow(
                 column(3, selectInput("province", "Province", choices = provinces, multiple=TRUE)
                        ),
                 column(5, sliderInput("date", "Date of observation", 
                                       floor_date(min(provdata$date),"hours"), 
                                       ceiling_date(Sys.time(),"hours"),
                                       value = c(floor_date(today()),ceiling_date(Sys.time(),"hours")),
                                       step=3600, ticks = T, width = 500)
                        ),
                 column(4, checkboxGroupInput("rarity", "Rarity of observation",
                                              choices = c("algemeen", "vrij algemeen","zeldzaam", "zeer zeldzaam"), 
                                              selected = c("algemeen", "vrij algemeen","zeldzaam", "zeer zeldzaam"),
                                              inline = T),
                        checkboxGroupInput("validity", "Validity of observation",
                                           choices = c("confirmed", "unknown","other"),
                                           selected = c("confirmed", "unknown","other"),
                                           inline = T)
                        )
             ),
             hr(),
             DT::dataTableOutput("obstable")
             
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
    
    
    liferdata <- reactive({
        if (input$lifer == 1){
           provdata[!provdata$species %in% lifelist(), ] # https://stackoverflow.com/questions/15227887
        } else {
           provdata
        }
    })
    
    filteredData <- reactive({
        from<- input$time[1]
        till<- input$time[2]
        liferdata() %>% filter(date >= from & date <=  till) %>% arrange(match(rarity, c("algemeen", "vrij algemeen", "zeldzaam", "zeer zeldzaam")))
    })
    
    labelcontent <- reactive({
        paste(sep="",
              "<b>",filteredData()$species,"</b>&emsp;[",filteredData()$rarity,"]",
              "<br/>Seen on ",as.POSIXct(filteredData()$date, tz = "CEST"),
              "<br/>Location: ",filteredData()$location,
              "<br/><a href=",filteredData()$link," target='_blank'>",filteredData()$link,"</a>")
    })
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addProviderTiles("OpenStreetMap.HOT", group = "OSM") %>%
            addProviderTiles("Esri.WorldImagery",group = "Satellite")%>%
            setView(lat = 52.2, lng = 5.5, zoom = 8) %>%
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
    
    #### DataTable
    output$obstable <- DT::renderDataTable({
        df <- provdata %>% 
            filter( date >= input$date[1],
                    date <= input$date[2],
                    is.null(input$province) | province %in% input$province,
                    is.null(input$rarity) | rarity %in% input$rarity,
                    is.null(input$validity) | validity %in% input$validity
                    ) %>% 
            select(-number, -lat, -lon, -province) %>%
            mutate(date = as.character(date)) %>%
            arrange(desc(date)) %>%
            mutate(link =  paste0("<a href='",link,"' target='_blank'>",link,"</a>"))
        DT::datatable(df, options = list(pageLength = 25), escape = F)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
