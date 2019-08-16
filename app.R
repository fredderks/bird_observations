#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2);library(dplyr);library(tidyr);library(data.table);library(leaflet);library(leaflet.extras)
library(lubridate);library(magrittr);library(stringr);library(purrr);library(shiny);library(rvest);library(glue)

myIcons <- iconList("algemeen" = makeIcon("green.png", iconWidth = 18, iconHeight = 24), 
                    "vrij algemeen" = makeIcon("blue.png", iconWidth = 18, iconHeight = 24),
                    "zeldzaam" = makeIcon("amber.png", iconWidth = 18, iconHeight =24),
                    "zeer zeldzaam" = makeIcon("red.png", iconWidth = 18, iconHeight =24))
file <- paste(sep="","LIFER_obs_",0,"_",today(),".rds")
provdata <- readRDS(file)
provdata$province <- sub(".* ", "", provdata$location)

provinces <- c("All Provinces"="", "Drenthe"="(DR)", "Flevoland"="(FL)", "Friesland"="(FR)",
               "Gelderland"="(GE)", "Groningen"="(GR)", "Limburg"="(LI)", "Noord-Brabant"="(NB)",
               "Noord-Holland"="(NH)", "Overijssel"="(OV)", "Zuid-Holland"="(ZH)", "Utrecht"="(UT)",
               "Zeeland"="(ZL)")

#### Define UI for application that draws a map plotting bird observations ####
ui <- navbarPage(span(tags$i(class = "fas fa-binoculars fa-lg"),style="font-size:140%;"), windowTitle = "Observations Map",
    tabPanel(span(tags$i(class = "fas fa-map-marked-alt fa-lg"),
                "Interactive Observations Map",style="font-size:130%;"),
        div(class="outer",
            tags$head(
                # Include our custom CSS
                includeCSS("styles.css"),
                tags$script(src = "message-handler.js"),
                tags$link(rel="shortcut icon", href="parroticon.png"),
                tags$script(src= "https://kit.fontawesome.com/0d5117e2fa.js")
                ),
        leafletOutput("mymap", width = "100%", height = "100%"),
        absolutePanel(top = 10, left = 70, width = 300,
                      wellPanel(
                          sliderInput("time", p(tags$i(class = "fas fa-calendar-alt"),"Date of observation"), 
                                      floor_date(today()-7), 
                                      ceiling_date(Sys.time(),"hours"),
                                      value = c(floor_date(today()),ceiling_date(Sys.time(),"hours")),
                                      step=3600, ticks = F),
                          textInput("userid",  p(tags$i(class = "fas fa-id-badge"),"Waarneming.nl User ID"), value = "130065", width = 170),
                          radioButtons("lifer","",
                                       choices = list("Only show LIFERs" = 1, "Show all observations" = 2), 
                                       selected = 1)
                          ), 
                      style = "opacity: 0.92" # https://shiny.rstudio.com/gallery/absolutely-positioned-panels.html
            )
        )
    ),
    tabPanel(span(tags$i(class = "fas fa-database fa-lg"),
                "Data Explorer", style="font-size:130%;"),
             fluidRow(
                 column(3, selectInput("province", "Province", choices = provinces, multiple=TRUE),
                        p(tags$i(class = "fas fa-edit"),glue('Last updated at: {file.info(file)$mtime}'))
                        ),
                 column(5, sliderInput("date", "Date of observation", 
                                       floor_date(min(provdata$date),"hours"), 
                                       ceiling_date(Sys.time(),"hours"),
                                       value = c(floor_date(today()),ceiling_date(Sys.time(),"hours")),
                                       step=3600, ticks = T, width = 600)
                        ),
                 column(1, checkboxGroupInput("rarity", "Rarity",
                                              choices = c("algemeen", "vrij algemeen","zeldzaam", "zeer zeldzaam"), 
                                              selected = c("algemeen", "vrij algemeen","zeldzaam", "zeer zeldzaam"))
                        ),
                 column(1, checkboxGroupInput("validity", "Validity",
                                              choices = c("confirmed", "unknown","other"),
                                              selected = c("confirmed", "unknown","other"))
                        ),
                 column(2, h4(a("Code is available on GitHub ",tags$i(class = "fab fa-github"), 
                               href = "https://github.com/fredderks/bird_observations"))
                 )
             ),
             # hr(),
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
