library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(rlist)
ui <- dashboardPage(
    skin="purple",
    dashboardHeader(title = strong("Covid 19")),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "Home",icon = icon("home")),
            menuItem("NYC", tabName = "NYC",menuSubItem("NYC Data",tabName = "nyc_data",icon=icon("chart-line","font-awesome")),menuSubItem("Bike",tabName = "Bike",icon = icon("biking","font-awesome")),menuSubItem("Hangout",tabName = "Hangout",icon=icon("glass-cheers","font-awesome")),icon = icon("city","font-awesome")),
            menuItem("World",tabName = "World",menuSubItem("World Data",tabName = "world_data",icon=icon("chart-line","font-awesome")),menuSubItem("Travel Policy",tabName = "policy",icon=icon("route","font-awesome")),menuSubItem("Flights",tabName="flight",icon=icon("plane-departure","font-awesome")),icon = icon("globe",lib = "font-awesome")),
            menuItem("About",tabName="About",icon=icon("question-circle","font-awesome"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Home",h2("introduction and what this project is about")),
            tabItem(tabName = "nyc_data",h2("NYC data")),
            tabItem(tabName = "Bike",h2("NYC bike")),
            tabItem(tabName = "Hangout",h2("NYC hangout")),
            tabItem(tabName = "world_data",h2("World data")),
            tabItem(tabName = "policy",h2("Travel policy")),
            tabItem(tabName = "flight",h2("Flight Search"),fluidPage(box(width = 12,
                div(style="display:inline-block",textInput("Departure","Departure From: ","",'190px',"New York")),
                div(style="display:inline-block",textInput("Destination","Destination: ","",'190px','Tokyo')),
                div(style="display:inline-block",textInput("Departure_Date","Departure Date: ","",'190px','yyyy-mm-dd')),
                div(style="display:inline-block",actionButton("Search",icon("refresh"),label="Search",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                verbatimTextOutput("value"),br(),
                div(infoBoxOutput("value2")),
                div(infoBoxOutput("value3")),
                div(infoBoxOutput("value4"))
            ))),
            tabItem(tabName = "About",h4("Group Member"))
        )
    )
)
# fluidRow(
#     box(width=12,splitLayout(
#         textInput("Departure","Departure From: ","",'180px',"New York"),
#         textInput("Destination","Destination: ","",'180px','Tokyo'),
#         textInput("Departure Date","Departure Date: ","",'180px','yyyy-mm-dd'),
#         textInput("Return Date","Return Date: ","",'175px','yyyy-mm-dd'),
#         submitButton("Search",icon("refresh"))
#     ))
# )
server <- function(input, output) {
    observeEvent(input$Search,{
        apikey = "8c0c1ea6bfmsh16e495f3796b012p1c9348jsne3d7672f66e9"
        # res = GET("https://tripadvisor1.p.rapidapi.com/airports/search",add_headers("x-rapidapi-host"="tripadvisor1.p.rapidapi.com","x-rapidapi-key"=apikey),query=list("locale" = "en_US", "query" = input$Departure))
        # res2 = GET("https://tripadvisor1.p.rapidapi.com/airports/search",add_headers("x-rapidapi-host"="tripadvisor1.p.rapidapi.com","x-rapidapi-key"=apikey),query=list("locale" = "en_US", "query" = input$Destination))
        # jsonres<-content(res,as="parsed") 
        # jsonres2<-content(res2,as="parsed")
        # # output$value <- renderPrint({ jsonres[[1]][[1]]})
        # departureairport <- list()
        # destinationairport <- list()
        # i <- 1
        # while (i < 4  & !(i > length(jsonres))){
        #     departureairport =list.append(departureairport,jsonres[[i]][[1]])
        #     i = i+ 1
        # }
        # j <- 1
        # while (j < 4  & !(j > length(jsonres2))){
        #     destinationairport =list.append(destinationairport,jsonres2[[j]][[1]])
        #     j = j+ 1
        # }
        # # output$value <- renderPrint({destinationairport})
        # res3 = GET("https://tripadvisor1.p.rapidapi.com/flights/create-session",add_headers("x-rapidapi-host"="tripadvisor1.p.rapidapi.com","x-rapidapi-key"=apikey),query=list("currency" = "USD", "ta"="1","c"="0","d1"=destinationairport[1],"o1"=departureairport[1],"dd1"=input$Departure_Date,"dd2"=input$Return_Date))
        # jsonres3<-content(res3,as="parsed")
        # # output$value <- renderPrint({jsonres3})
        # sid <- jsonres3[["search_params"]]["sid"]
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

# Run the application 
shinyApp(ui,server)
