source("global.R")
source("config.R")


server <- function(input, output) {
    
    #create summary box for total covid case in the world as summary
    output$covid_case_world <- renderValueBox({
        #-----need dataframe value------#
        valueBox(totalactivecases_data, subtitle = 'Total cases in the world', 
                 icon = icon('globe-americas'), 
                 color = 'light-blue')})
    output$covid_case_nyc <- renderValueBox({
        #-----need dataframe value------#
        valueBox(nyc_cases, subtitle = 'Total cases in NYC', 
                 icon = icon('city'), 
                 color = 'teal')
    })
    output$countries_without_travel_bans <- renderValueBox({
        valueBox(total_banall, subtitle = 'Countries with travel bans', 
                 icon = icon('flag'), 
                 color = 'olive')
        
        
        
    })
    
    output$panel1 <- renderUI({
        div(br(), box(width = 15,  'This tab shows each country’s most recent Covid-19 cases and related policies across the world so you can figure out where you can travel during the pandemic. When you’ve chosen a country you can and want to travel to, find the best priced flight with real-time flight information, including departure time and fare price, to finalize your next international voyage. ',
                      br(),  
                      tags$div(tags$ul(
                          tags$li("Policies: continuously updated latest government policies pulled from Oxford Covid-19 Government Response Tracker."),
                          tags$li("Filter by 1 criteria: heatmap of every country’s policies/ Covid-19 data with detailed notes on policy of interest. Users can also filter out countries that adopt a certain policy (e.g. no measure) within each criteria (e.g. International Travel) by selecting that policy from the dropdown."),
                          tags$li("Filter by Multiple criteria: After selecting values for each and every criteria of interest, the map will highlight those that meet the requirements."),
                          tags$li("Filter by Countries: Select countries of interest. Hover over each country for a list of all its policies and Covid-19 data.")
                      ))
        )
        )
    })
    output$panel2 <- renderUI({
        div(br(), box(width = 15, 'Even if you can’t travel internationally right now, you can still travel, explore, and hang out around New York City. This tab contains a map of New York City that allows you to find your nearest park or preferred outdoor space, social-distance spots and dining, bike stations, bike lanes, as well as stay up-to-date on Covid-19 cases in New York City.', br(),  
                      tags$div(tags$ul(
                          tags$li("Open Street data: streets closed off to cars in NYC, for pedistriations to hang-out and restaurants to create outdoor dining during Covid-19."),
                          tags$li("Citibike: shows every Citibike, easy-to-use pay-by-ride bikes, station in New York City."),
                          tags$li("Parks: New York City parks."),
                          tags$li("Directions: find the fastest walking path to get from where you are in the city to where you want to go."),
                          tags$li("Covid-19: up-to-date information of Covid-19 cases in each zip code in New York City.")
                      ))) 
        )
    })
    
    observeEvent("", {
        show('world_panel')
        hide('nyc_panel')
        
    }, once = TRUE)
    
    observeEvent(input$World_covid_case, {
        shinyjs::show('world_panel')
        shinyjs::hide('nyc_panel')
        
    })
    observeEvent(input$NYC_covid_case, {
        shinyjs::show('nyc_panel')
        shinyjs::hide('world_panel')
        
    })
    
    
    
    
    
    
    
#Kristen's part    
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
            
#Elise+ Jaival's part
        observe({
            
            if(input$filter =='Countries'){
                selected_countries <-reactive(as.vector(latest_orig$CountryName%in%input$Countries))
                
                    latest_orig<-mutate(latest_orig, Selected=ifelse(selected_countries(), 'Yes',NA))
                    
                    countries_join <- merge(countries,
                                            latest_orig,
                                            by.x = 'NAME',
                                            by.y = 'CountryName',sort = FALSE)
                    write.csv(countries_join,file= '../app/output/countries_join.csv')
                    
                    output$map <- renderLeaflet({
                        leaflet(countries) %>%setView(0, 30, zoom = 2) })
                            # addProviderTiles("Mapbox", options = providerTileOptions(id = "mapbox.light",
                            #                                                          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
                            
                    
                    #pop up for polygons: If you click on a country, it shows you the country name and Total cases like below
                    country_popup <- paste0("<strong>Country: </strong>",countries$NAME,
                                            "<br>", "International Travel: ",countries_join$intl_travel.value,"<br>",
                                            "<br>", "Domestic Movement: ",countries_join$dom_move.value,"<br>",
                                            "<br>", "Stay at Home: ",countries_join$stayhome.value,"<br>",
                                            "<br>", "Gathering: ",countries_join$gathering.value,"<br>",
                                            "<br>", "Public Transport: ",countries_join$public_transport.value,"<br>",
                                            "<br>", "Active Cases: ",countries_join$Current,"<br>",
                                            "<br>", "Percentage Change: ",countries_join$Change,"<br>")
                    
                    factpal <- colorFactor(topo.colors(1), countries_join$Selected)
                    
                    proxy<- leafletProxy("map", data = countries_join)%>%
                        addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                    fillColor = ~factpal(Selected),color = 'black',weight = "0.7", opacity=1,
                                    highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                    layerId = ~NAME,popup = country_popup)
                #})
            }
            
            else if(input$filter =='1 criterion')
            {
                #Create heatmaps by policy types
                heatmap_data <- reactive({
                    if(input$heatmap == "International Travel"){
                        return(latest_intl_travel)
                    }else if(input$heatmap == "Domestic Movement"){
                        return(latest_dom_move)
                    }else if(input$heatmap == "Stay at Home"){
                        return(latest_stayhome)
                    }else if(input$heatmap == "Gathering Restrictions"){
                        return(latest_gathering)
                    }else if(input$heatmap == "Public Transport"){
                        return(latest_public_transport)
                    }else if(input$heatmap == "Current Cases"){
                        return(currentcases)}
                })
                
                if(input$heatmap == "Current Cases"){
                    currentcases_join <- merge(countries,
                                               heatmap_data(),
                                               by.x = 'NAME',
                                               by.y = 'CountryName',sort = FALSE)
                    write.csv(currentcases_join, file = 'currentcases_join.csv')
                    cases_val <- reactive({return(input$covidcases)})
                    output$map <- renderLeaflet({
                        leaflet(countries) %>% 
                           # addProviderTiles("Mapbox", options = providerTileOptions(id = "mapbox.light",
                            #                                                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
                            setView(0, 30, zoom = 2)
                    })
                    if(input$covidcases == 'Daily Count'){
                        country_popup <- paste0("<strong>Country: </strong>",
                                                currentcases_join$NAME,
                                                "<br>", "Cases: ",currentcases_join$Current,"<br>")
                        bins <- c(0, 50, 100, 500, 1000, 5000, 20000, 50000, 100000,Inf)
                        pal <- colorBin("YlOrRd", domain = currentcases_join$Current, bins = bins)
                        proxy <- leafletProxy("map",data = currentcases_join) %>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~pal(Current),color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "white",weight = 1,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup) %>% clearControls()%>%
                            addLegend("topleft", pal = pal,values = ~Current, title = "Cases",opacity = 1)
                        
                    }else if(input$covidcases != 'Daily Count'){
                        country_popup <- paste0("<strong>Country: </strong>",
                                                currentcases_join$NAME,
                                                "<br>", "Percentage Change: ",currentcases_join$Change,"<br>")
                        bins <- c(-500, -100, 0, 10, 30, 50, 100,Inf)
                        pal <- colorBin("YlOrRd", domain = currentcases_join$Change, bins = bins)
                        proxy <- leafletProxy("map",data = currentcases_join) %>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~pal(Change),color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "white",weight = 1,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup) %>% clearControls()%>%
                            addLegend("topleft", pal = pal,values = ~Change, title = "Cases",opacity = 1)
                    }
                }
                
                if(input$heatmap == "International Travel")
                {
                    #merge the spatial dataframe and cases dataframe
                    intl_travel_join <- merge(countries,
                                              heatmap_data(),
                                              by.x = 'NAME',
                                              by.y = 'CountryName',sort = FALSE)
                    write.csv(intl_travel_join, file = '../app/output/Intl_Travel_join.csv')
                    
                    #policy_val <- reactive({return(input$intl_travel)})
                    
                    output$map <- renderLeaflet({
                        leaflet(countries) %>%
                            # addProviderTiles("Mapbox", options = providerTileOptions(id = "mapbox.light",
                            #                                                          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
                            setView(0, 30, zoom = 2) })
                    
                    if(input$intl_travel=='All')
                    {
                        #pop up for polygons: If you click on a country, it shows you the country name and Total cases like below
                        country_popup <- paste0("<strong>Country: </strong>",
                                                intl_travel_join$NAME,
                                                "<br>", "Note: ",intl_travel_join$intl_travel.Notes,"<br>")
                        
                        factpal <- colorFactor(topo.colors(5), intl_travel_join$intl_travel.value)
                        
                        proxy<- leafletProxy("map", data = intl_travel_join)%>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~factpal(intl_travel.value), color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup)
                        proxy %>% clearControls()
                        proxy %>% addLegend("topleft", pal = factpal,values = ~intl_travel.value, labels = c('no restrictions','screening arrivals',
                                                                                                             'quarantine arrivals from some or all regions',
                                                                                                             'ban arrivals from some regions',
                                                                                                             'ban on all regions or total border closure'),
                                            title = "Policy",opacity = 1)
                    }else if (input$intl_travel!='All'){
                        intl_travel_filter <- intl_travel_join
                        intl_travel_filter$intl_travel.value[intl_travel_filter$intl_travel.value!=input$intl_travel] <- NA
                        write.csv(intl_travel_filter, file = '../app/output/Intl_Travel_filter.csv')
                        
                        country_popup <- paste0("<strong>Country: </strong>",
                                                intl_travel_filter$NAME,
                                                "<br>","Note: ", intl_travel_filter$intl_travel.Notes,"<br>")
                        
                        factpal <- colorFactor(topo.colors(1), intl_travel_filter$intl_travel.value)
                        
                        leafletProxy("map", data = intl_travel_filter)%>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~factpal(intl_travel.value),color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup)
                    }
                }
                else if (input$heatmap == "Domestic Movement")
                {
                    dom_move_join<- merge(countries,heatmap_data(),by.x = 'NAME',by.y = 'CountryName',sort = FALSE)
                    write.csv(dom_move_join, file = '../app/output/Dom_move_join.csv')
                    
                    if(input$dom_move=='All')
                    {
                        #pop up for polygons
                        country_popup <- paste0("<strong>Country: </strong>",
                                                dom_move_join$NAME,
                                                "<br>","Note: ",dom_move_join$dom_move.Notes,"<br>")
                        
                        factpal2 <- colorFactor(topo.colors(3), dom_move_join$dom_move.value)
                        proxy<-leafletProxy("map", data = dom_move_join)%>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~factpal2(dom_move.value),color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup)
                        proxy %>% clearControls()
                        proxy %>% addLegend("topleft",pal = factpal2,values = ~dom_move.value,
                                            labels = c('no measures','recommend not to travel between regions/cities',
                                                       'internal movement restrictions in place'),title = "Policy",opacity = 1)
                        
                    }
                    else if(input$dom_move!='All')
                    {
                        dom_move_filter <- dom_move_join
                        dom_move_filter$dom_move.value[dom_move_filter$dom_move.value!=input$dom_move] <- NA
                        write.csv(dom_move_filter, file = '../app/output/Dom_move_filter.csv')
                        
                        country_popup <- paste0("<strong>Country: </strong>",
                                                dom_move_filter$NAME,
                                                "<br>","Note: ",dom_move_filter$Notes,"<br>")
                        
                        factpal <- colorFactor(topo.colors(1), dom_move_filter$dom_move.value)
                        
                        leafletProxy("map", data = dom_move_filter)%>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~factpal(dom_move.value),color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup)
                    }
                }
                else if (input$heatmap == "Stay at Home")
                {
                    stayhome_join<- merge(countries,heatmap_data(),by.x = 'NAME',by.y = 'CountryName',sort = FALSE)
                    write.csv(stayhome_join, file = '../app/output/Stayhome_join.csv')   
                    if(input$stayhome=='All')
                    {
                        #pop up for polygons
                        country_popup <- paste0("<strong>Country: </strong>",
                                                stayhome_join$NAME,
                                                "<br>","Note: ",stayhome_join$stayhome.Notes, "<br>")
                        
                        factpal2 <- colorFactor(topo.colors(4), stayhome_join$stayhome.value)
                        proxy<-leafletProxy("map", data = stayhome_join)%>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~factpal2(stayhome.value),color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup)
                        proxy%>%clearControls()
                        proxy%>%addLegend("topleft",pal = factpal2,values = ~stayhome.value, labels = c('0' = 'no measures', '1' = 'recommend not leaving house',
                                                                                                        '2' = 'require not leaving house with exceptions for daily exercise, grocery shopping, and essential trips',
                                                                                                        '3' = 'require not leaving house with minimal exceptions'),title = "Policy",opacity = 1)
                        
                    }
                    else if(input$stayhome!='All')
                    {
                        stayhome_filter <- stayhome_join
                        stayhome_filter$stayhome.value[stayhome_filter$stayhome.value!=input$stayhome] <- NA
                        write.csv(stayhome_filter, file = '../app/output/Stayhome_filter.csv')
                        
                        country_popup <- paste0("<strong>Country: </strong>",
                                                stayhome_join$NAME,
                                                "<br>","Note: ",stayhome_join$stayhome.Notes, "<br>")
                        
                        factpal <- colorFactor(topo.colors(1), stayhome_filter$value)
                        
                        leafletProxy("map", data = stayhome_filter)%>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~factpal(stayhome.value),color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup)
                    }
                }
                else if (input$heatmap == "Gathering Restrictions")
                {
                    gathering_join<- merge(countries,
                                           heatmap_data(),
                                           by.x = 'NAME',
                                           by.y = 'CountryName',
                                           sort = FALSE)
                    
                    if(input$gathering=='All'){
                        
                        #pop up for polygons
                        country_popup <- paste0("<strong>Country: </strong>",
                                                gathering_join$NAME,
                                                "<br><strong>",
                                                "Note: ",
                                                gathering_join$gathering.Notes,
                                                "<br><strong>")
                        factpal2 <- colorFactor(topo.colors(4),gathering_join$gathering.value) 
                                                
                        proxy<-leafletProxy("map", data = gathering_join)%>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~factpal2(gathering.value),color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup)
                        proxy%>%clearControls()
                        proxy%>% addLegend("topleft",pal = factpal2,values = ~gathering.value, labels = c('0' = 'no restrictions',
                                                                                                          '1' = 'restrictions on very large gatherings (the limit is above 1000 people)',
                                                                                                          '2' = 'restrictions on gatherings between 101-1000 people',
                                                                                                          '3' = 'restrictions on gatherings between 11-100 people',
                                                                                                          '4' = 'restrictions on gatherings of 10 people or less'),title = "Policy",opacity = 1)
                        
                    }
                    else
                    {
                        gathering_filter <- gathering_join
                        gathering_filter$gathering.value[gathering_filter$gathering.value!=input$gathering] <- NA
                        write.csv(gathering_filter, file = '../app/output/Gathering_filter.csv')
                        
                        country_popup <- paste0("<strong>Country: </strong>",
                                                gathering_filter$NAME,
                                                "<br><strong>",
                                                "Note: ",
                                                gathering_filter$gathering.Notes,
                                                "<br><strong>")
                        
                        factpal <- colorFactor(topo.colors(1), gathering$value)
                        
                        proxy<-leafletProxy("map", data = gathering_filter)%>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~factpal(gathering.value),color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup)
                    }
                }
                
                else if (input$heatmap == "Public Transport")
                {
                    publictrans_join<- merge(countries,
                                             heatmap_data(),
                                             by.x = 'NAME',
                                             by.y = 'CountryName',
                                             sort = FALSE)
                    if(input$pub_trans=='All'){
                        
                        #pop up for polygons
                        country_popup <- paste0("<strong>Country: </strong>",
                                                publictrans_join$NAME,
                                                "<br><strong>",
                                                "Note: ",
                                                publictrans_join$public_transport.Notes,
                                                "<br><strong>")
                        factpal2 <- colorFactor(topo.colors(4), publictrans_join$public_transport.value)
                        proxy<-leafletProxy("map", data = publictrans_join)%>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~factpal2(public_transport.value),color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup)
                        proxy%>%clearControls()
                        proxy%>% addLegend("topleft",pal = factpal2,values = ~public_transport.value, labels = c('0' = 'no measures',
                                                                                                                 '1' = 'recommend closing (or significantly reduce volume/route/means of transport available)',
                                                                                                                 '2' = 'require closing (or prohibit most citizens from using it)'),title = "Policy",opacity = 1)
                    }
                    else
                    {
                        publictrans_filter <- publictrans_join
                        publictrans_filter$public_transport.value[publictrans_filter$public_transport.value!=input$pub_trans] <- NA
                        write.csv(publictrans_filter, file = '../app/output/Publictrans_filter.csv')
                        
                        country_popup <- paste0("<strong>Country: </strong>",
                                                publictrans_filter$NAME,
                                                "<br><strong>",
                                                "Note: ",
                                                publictrans_filter$public_transport.Notes,
                                                "<br><strong>")
                        
                        factpal <- colorFactor(topo.colors(1), publictrans_filter$public_transport.value)
                        leafletProxy("map", data = publictrans_filter)%>%
                            addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                        fillColor = ~factpal(public_transport.value),color = 'black',weight = "0.7", opacity=1,
                                        highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                        layerId = ~NAME,popup = country_popup)
                    }
                }
            }
            
            else if(input$filter =='Multiple criteria')
            {
                
                #Process Latest_all policies dataset:
                latest_all = merge(latest_intl_travel, latest_dom_move, by= 'CountryName',duplicateGeoms = TRUE)
                latest_all <-merge(latest_all,latest_stayhome,by= 'CountryName',duplicateGeoms = TRUE)
                latest_all <-merge(latest_all,latest_gathering,by= 'CountryName',duplicateGeoms = TRUE)
                latest_all <-merge(latest_all,latest_public_transport,by= 'CountryName',duplicateGeoms = TRUE)
                write.csv(latest_all, file = '../app/output/Latest_all.csv')
                
                #Subset latest_all by International Travel value option
                policy_data <- reactive({
                    if('International Travel'%in%input$choices){
                        latest_all<-latest_all%>%filter(intl_travel.value==input$intl_travel)}
                    if('Domestic Movement'%in%input$choices){
                        latest_all<-latest_all%>%filter(dom_move.value==input$dom_move)}
                    if('Stay at Home'%in%input$choices){
                        latest_all<-latest_all%>%filter(stayhome.value==input$stayhome)}
                    if('Gathering Restrictions'%in%input$choices){
                        latest_all<-latest_all%>%filter(gathering.value==input$gathering)}
                    if('Public Transport'%in%input$choices){
                        latest_all<-latest_all%>%filter(public_transport.value==input$pub_trans)}
                    return (latest_all)})
                
                observe({
                    policy_join <- merge(countries,
                                         policy_data(),
                                         by.x = 'NAME',
                                         by.y = 'CountryName',sort = FALSE)
                    write.csv(policy_join, file = '../app/output/Policy_join.csv')
                    
                    output$map <- renderLeaflet({
                        leaflet(countries) %>%
                            # addProviderTiles("Mapbox", options = providerTileOptions(id = "mapbox.light",
                            #                                                          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
                            setView(0, 30, zoom = 2) })
                    
                    #pop up for polygons: If you click on a country, it shows you the country name and Total cases like below
                    country_popup <- paste0("<strong>Country: </strong>",
                                            policy_join$NAME)
                    
                    factpal <- colorFactor(topo.colors(1), policy_join$intl_travel.value)
                    
                    proxy<- leafletProxy("map", data = policy_join)%>%
                        addPolygons(stroke = TRUE, smoothFactor = 0.2, fillOpacity = 1,
                                    fillColor = ~factpal(intl_travel.value),color = 'black',weight = "0.7", opacity=1,
                                    highlightOptions = highlightOptions(color = "red",weight = 3,bringToFront = TRUE),
                                    layerId = ~NAME,popup = country_popup)
                })
            }
        })     
    
#Sophie's part
        observeEvent(input$Search,{
            apikey = "9365b5afafmsh81270de0cb79831p113144jsn2ea3dd851a6a"
            res = GET("https://tripadvisor1.p.rapidapi.com/airports/search",add_headers("x-rapidapi-host"="tripadvisor1.p.rapidapi.com","x-rapidapi-key"=apikey),query=list("locale" = "en_US", "query" = input$Departure))
            res2 = GET("https://tripadvisor1.p.rapidapi.com/airports/search",add_headers("x-rapidapi-host"="tripadvisor1.p.rapidapi.com","x-rapidapi-key"=apikey),query=list("locale" = "en_US", "query" = input$Destination))
            jsonres<-content(res,as="parsed")
            jsonres2<-content(res2,as="parsed")
            departureairport <- list()
            destinationairport <- list()
            i <- 1
            while (i < 4  & !(i > length(jsonres))){
                departureairport =list.append(departureairport,jsonres[[i]][[1]])
                i = i+ 1
            }
            j <- 1
            while (j < 4  & !(j > length(jsonres2))){
                destinationairport =list.append(destinationairport,jsonres2[[j]][[1]])
                j = j+ 1
            }
            sid <- ""
            jsonres3 <- ""
            m <- 1
            while(m < 3){
                res3 = GET("https://tripadvisor1.p.rapidapi.com/flights/create-session",add_headers("x-rapidapi-host"="tripadvisor1.p.rapidapi.com","x-rapidapi-key"=apikey),query=list("currency" = "USD", "ta"="1","c"="0","d1"=destinationairport[1],"o1"=departureairport[1],"dd1"=input$Departure_Date,"dd2"=input$Return_Date))
                jsonres3<-content(res3,as="parsed")
                sid = jsonres3[["search_params"]]["sid"]
                if(sid != "NULL"){
                    break
                }
                m = m+1
            }
            if (!(is.null(sid)) & (sid != "")){
                takeofftime <- ""
                n <- 1
                while((length(takeofftime == 0) | is.null(takeofftime)) & n < 3){
                    res4 = GET("https://tripadvisor1.p.rapidapi.com/flights/poll",add_headers("x-rapidapi-host"="tripadvisor1.p.rapidapi.com","x-rapidapi-key"=apikey),query=list("sid" = sid, "currency"="USD","n"="15","ns"="NON_STOP","so"="PRICE","o"="0"))
                    jsonres4<-content(res4,as="parsed")
                    takeofftime <- jsonres4[["itineraries"]][[1]][["f"]][[1]][["l"]][[1]]["dd"]
                    n = n+1
                }
                listrender <- list()
                if(!is.null(takeofftime)){
                    p <- 1
                    v <- list()
                    while(p < 5 & p < length(jsonres4[["itineraries"]])+1){
                        price <- jsonres4[["itineraries"]][[p]][["l"]][[1]][["pr"]]["dp"]
                        airline <- jsonres4[["itineraries"]][[p]][["l"]][[1]]["s"]
                        takeoff <- jsonres4[["itineraries"]][[p]][["f"]][[1]][["l"]][[1]]["da"]
                        land = jsonres4[["itineraries"]][[p]][["f"]][[1]][["l"]][[1]]["aa"]
                        takeofftime <- jsonres4[["itineraries"]][[p]][["f"]][[1]][["l"]][[1]]["dd"]
                        landtime <- jsonres4[["itineraries"]][[p]][["f"]][[1]][["l"]][[1]]["ad"]
                        distance <- jsonres4[["itineraries"]][[p]][["f"]][[1]][["l"]][[1]]["di"]
                        splittakeoff <- strsplit(takeofftime[[1]],"T")
                        splittakeoff <- strsplit(splittakeoff[[1]][2],":")
                        takeofftime <- paste(splittakeoff[[1]][1],splittakeoff[[1]][2],sep=":")
                        splitlandtime <- strsplit(landtime[[1]],"T")
                        splitlandtime <- strsplit(splitlandtime[[1]][2],":")
                        landtime <- paste(splitlandtime[[1]][1],splitlandtime[[1]][2],sep=":")
                        v[[p]] <- box(width=12,
                                      list(
                                          infoBox(
                                              h4(airline),
                                              paste(takeoff,land,sep="-"),
                                              width=4,
                                              color = "teal",
                                              icon = icon("plane","font-awesome")
                                          ),
                                          infoBox(
                                              h4("Flight Time:"),
                                              paste(takeofftime,landtime,sep="-"),
                                              width=4,
                                              color = "blue",
                                              icon=icon("clock","font-awesome")
                                          ),
                                          infoBox(
                                              h4("Price:"),
                                              price,
                                              width=4,
                                              color = "green",
                                              icon=icon("dollar-sign","font-awesome")
                                          )
                                      ))
                        p=p+1
                    }
                    output$value2 <- renderPrint({paste("Check out the results below ",Sys.time())})
                    output$ticket <- renderUI(v)
                }else{
                    output$value2 <- renderPrint({paste("API error. Try again later.",Sys.time())})
                }
            }else{
                output$value2 <- renderPrint({paste("No flight found. Try another date.",Sys.time(),sid,sep=" ")})
            }
        }
        )
}


