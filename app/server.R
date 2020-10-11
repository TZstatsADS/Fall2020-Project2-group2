source("global.R")
source("config.R")

server <- function(input, output) {
    
        output$nyc_map <- renderLeaflet({

            leaflet("nyc_map") %>%
                addTiles() %>% setView(lng = -73.98928, lat = 40.75042,zoom=11) %>%
                addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE))

            })
        

        output$text2 <- renderUI({
            HTML("Click where you are on the map,<br/>then click where you want to go.")
        })
        
        proxy <- leafletProxy("nyc_map")
        
        observe({
            if (input$open_street == T) {
                proxy %>% showGroup("Open Streets")
            } else {
                proxy %>% hideGroup("Open Streets")
            }
            
            if (input$citibike == T) {
                proxy %>% showGroup("CitiBike Stations")
            } else {
                proxy %>% hideGroup("CitiBike Stations")
            }
            
            if (input$parktype == "Park" & input$parks==T) {
                proxy %>% showGroup("parkfiltered")
            } else {
                proxy %>% hideGroup("parkfiltered")
            }
            
            if (input$parktype == "Playground"& input$parks==T) {
                proxy %>% showGroup("playgroundfiltered")
            } else {
                proxy %>% hideGroup("playgroundfiltered")
            }
            
            if (input$parktype == "Nature Area"& input$parks==T) {
                proxy %>% showGroup("naturefiltered")
            } else {
                proxy %>% hideGroup("naturefiltered")
            }
            
            if (input$parktype == "Triangle/Plaza"& input$parks==T) {
                proxy %>% showGroup("plazafiltered")
            } else {
                proxy %>% hideGroup("plazafiltered")
            }
            
            if (input$parktype == "Parkway"& input$parks==T) {
                proxy %>% showGroup("parkwayfiltered")
            } else {
                proxy %>% hideGroup("parkwayfiltered")
            }
            
            if (input$parktype == "Recreation Field/Courts"& input$parks==T) {
                proxy %>% showGroup("recreationfiltered")
            } else {
                proxy %>% hideGroup("recreationfiltered")
            }
            
            if (input$bike_lanes == T) {
                proxy %>% showGroup("bikelanes")
            } else {
                proxy %>% hideGroup("bikelanes")
            }
            
            if (input$covid == T) {
                proxy %>% showGroup("covidcases")
            } else {
                proxy %>% hideGroup("covidcases")
            }
        })
        
        
        
        filteredstreets <- reactive({
            req(input$open_street)
            req(input$openstreetday)
            req(input$timesopen)
            
            if (input$openstreetday=="Monday") {
                open_street %>%filter(M_open <= input$timesopen[1] & M_end>= input$timesopen[2])
            }
            
            else if (input$openstreetday=="Tuesday") {
                open_street %>%filter(T_open <= input$timesopen[1] & T_end>= input$timesopen[2])
            }
            
            else if (input$openstreetday=="Wednesday") {
                open_street %>%filter(W_open <= input$timesopen[1] & W_end>= input$timesopen[2])
            }
            
            else if (input$openstreetday=="Thursday") {
                open_street %>%filter(R_open <= input$timesopen[1] & R_end>= input$timesopen[2])
            }
            
            else if (input$openstreetday=="Friday") {
                open_street %>% 
                    filter(F_open <= input$timesopen[1] & F_end>= input$timesopen[2])
            }
            
            else if (input$openstreetday=="Saturday") {
                open_street %>%filter(S_open <= input$timesopen[1] & S_end>= input$timesopen[2])
            }
            
            else if (input$openstreetday=="Sunday") {
                open_street %>%filter(U_open <= input$timesopen[1] & U_end>= input$timesopen[2])
            }
            
        })
        
        # observeEvent(input$open_street,{
        observe({
            label_streets <- sprintf(
                "From Street: <strong>%s</strong><br/> To Street: <strong>%s</strong><br/> Type: <strong>%s</strong>",
                open_street$from_stree, open_street$to_street, open_street$type
            ) %>% lapply(htmltools::HTML)
            
            leafletProxy("nyc_map") %>% 
                clearGroup('Open Streets') %>%
                addPolygons(data=filteredstreets(), label = label_streets, weight=2,  col = 'red', group = "Open Streets")
        })
        
            
        observeEvent(input$covid,{
            
            labels_zip <- sprintf(
                "<strong>%g%%</strong><br/>Neighborhood: <strong>%s</strong><br/>Zip Code: <strong>%s</strong>",
                round(char_zips.use@data$COVID_CASE_COUNT/1000,2), char_zips.use@data$NEIGHBORHOOD_NAME, char_zips.use@data$GEOID10
            ) %>% lapply(htmltools::HTML)
            
            pal <- colorNumeric(
                palette = "Greens",
                domain = char_zips.use@data$COVID_CASE_COUNT/1000)
            
            leafletProxy("nyc_map") %>% 
                addPolygons(data=char_zips.use, label=labels_zip, fillColor = ~pal(COVID_CASE_COUNT/1000), group = "covidcases", weight=1, col = 'black')
            
        })
        
        observeEvent(input$parks,{
            label_parks <- sprintf(
                "<strong>%s</strong>",
                parks$park_name
            ) %>% lapply(htmltools::HTML)
            
            leafletProxy("nyc_map") %>% 
                addPolygons(data=parks[parks$landuse=="Park",],label= label_parks, weight=1, col = 'green', group = "parkfiltered") %>%
                addPolygons(data=parks[parks$landuse=="Playground",],label= label_parks, weight=1, col = 'green', group = "playgroundfiltered") %>%
                addPolygons(data=parks[parks$landuse=="Nature Area",],label= label_parks, weight=1, col = 'green', group = "naturefiltered") %>%
                addPolygons(data=parks[parks$landuse=="Parkway",],label= label_parks, weight=1, col = 'green', group = "parkwayfiltered") %>%
                addPolygons(data=parks[parks$landuse=="Triangle/Plaza",],label= label_parks, weight=1, col = 'green', group = "plazafiltered") %>%
                addPolygons(data=parks[parks$landuse=="Recreation Field/Courts",],label= label_parks, weight=1, col = 'green', group = "recreationfiltered") 
            
        })
        
        observeEvent(input$citibike, {
            
            leafletProxy("nyc_map") %>% 
            addMarkers(data=citibike[citibike$region_id=="71",],lng=~lon,lat=~lat,label = ~name,
                       clusterOptions = markerClusterOptions(), group = "CitiBike Stations") 
        })
        
        observeEvent(input$bike_lanes, {
            label_bikelanes <- sprintf(
                "<strong>%s</strong>",
                bike_lanes$street
            ) %>% lapply(htmltools::HTML)
            
            leafletProxy("nyc_map") %>% 
                addPolygons(data=bike_lanes,weight=2, label = label_bikelanes, col = 'blue',  group = "bikelanes") 
                
        })

        RV<-reactiveValues(Clicks=list())
        observeEvent(input$nyc_map_click, {
            
            if (input$directions==T) {
                
                click <- input$nyc_map_click
                leafletProxy('nyc_map')%>%addMarkers(lng = click$lng, lat = click$lat)
                RV$Clicks <- c(RV$Clicks, list(click$lat, click$lng))
                origin_pt <- c(RV$Clicks[[1]], RV$Clicks[[2]])
                
                if (length(RV$Clicks) > 2 ) {
                    destination_pt <- c(RV$Clicks[[3]], RV$Clicks[[4]])
                    
                }
                
                if  (length(RV$Clicks) == 4 ) {
                    locations <- data.frame(lng = c(as.numeric(origin_pt[2]), as.numeric(destination_pt[2])),
                                            lat = c(as.numeric(origin_pt[1]), as.numeric(destination_pt[1])))
                    
                    
                    dir <- ors_directions(coordinates=  locations, profile="foot-walking",
                                          output = "sf",
                                          api_key = ors_key)
                    leafletProxy("nyc_map") %>% addPolygons(data=dir,group = "directions",layerId = "dir")%>% 
                        showGroup("directions")
                    
                }
                
                if  (length(RV$Clicks) > 4 ) {
                    leafletProxy('nyc_map')%>%clearMarkers()%>% hideGroup("directions")
                    RV$Clicks <- NULL
                }
                
            }
            
        })
            
   
    

    observeEvent(input$Search,{
        apikey = flight_api_key

        tempsid = "2704a331-fc0b-48f1-83c3-73ab643768db.313"
        res4 = GET("https://tripadvisor1.p.rapidapi.com/flights/poll",add_headers("x-rapidapi-host"="tripadvisor1.p.rapidapi.com","x-rapidapi-key"=apikey),query=list("sid" = tempsid, "currency"="USD","n"="15","ns"="NON_STOP","o"="0"))
        jsonres4<-content(res4,as="parsed")
        price <-jsonres4[["itineraries"]][[1]][["l"]][[1]][["pr"]]["dp"]
        airline <- jsonres4[["itineraries"]][[1]][["l"]][[1]]["s"]
        takeoff <- jsonres4[["itineraries"]][[1]][["f"]][[1]][["l"]][[1]]["da"]
        land <- jsonres4[["itineraries"]][[1]][["f"]][[1]][["l"]][[1]]["aa"]
        takeofftime <- jsonres4[["itineraries"]][[1]][["f"]][[1]][["l"]][[1]]["dd"]
        landtime <- jsonres4[["itineraries"]][[1]][["f"]][[1]][["l"]][[1]]["ad"]
        distance <- jsonres4[["itineraries"]][[1]][["f"]][[1]][["l"]][[1]]["di"]
        # output$value <- renderPrint({distance})
        splittakeoff <- strsplit(takeofftime[[1]],"T")
        splittakeoff = strsplit(splittakeoff[[1]][2],":")
        takeofftime = paste(splittakeoff[[1]][1],splittakeoff[[1]][2],sep=":")
        splitlandtime <- strsplit(landtime[[1]],"T")
        splitlandtime = strsplit(splitlandtime[[1]][2],":")
        landtime = paste(splitlandtime[[1]][1],splitlandtime[[1]][2],sep=":")
        # output$value <- renderPrint({landtime})
        output$value2 <- renderInfoBox({
            infoBox(
                h4(airline),
                paste(takeoff,land,sep="-"),
                width=1,
                color = "teal",
                icon = icon("plane","font-awesome")
            )
        })
        output$value3 <- renderInfoBox({
            infoBox(
                h4("Flight Time:"),
                paste(takeofftime,landtime,sep="-"),
                width=1,
                color = "blue",
                icon=icon("clock","font-awesome")
            )
        })
        output$value4 <- renderInfoBox({
            infoBox(
                h4("Price:"),
                price,
                width=1,
                color = "green",
                icon=icon("dollar-sign","font-awesome")
            )
        })
    }
    )
}


