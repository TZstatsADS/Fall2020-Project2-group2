
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
      menuItem("World",tabName = "World",menuSubItem("World Data",tabName = "world_data",icon=icon("chart-line","font-awesome")),menuSubItem("Travel Policy",tabName = "policy",icon=icon("route","font-awesome")),menuSubItem("Flights",tabName="flight",icon=icon("plane-departure","font-awesome")),icon = icon("globe",lib = "font-awesome")),
      menuItem("New York City", tabName = "New York City",menuSubItem("Travel NYC",tabName = "nyc_data",icon=icon("chart-line","font-awesome")),icon = icon("city","font-awesome")),
      menuItem("About",tabName="About",icon=icon("question-circle","font-awesome"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",h2("introduction and what this project is about")),
      tabItem(tabName = "nyc_data",
              titlePanel("Where Can I Travel or Hang Out (Social Distance Style) in New York City?"),
              h5("Even if you are unable to travel internationally right now, there are still many things to do safely in New York City."),
              h5("New Yorkers have access to Open Streets, which includes over 100 miles of streets closed off to cars to allow for greater social distancing during Covid-19. Explore and locate the nearest Open Street near you to meet up with your friends for a spacious chat or outdoor dining. New Yorkers also have access to over 100 different parks, recreational centers, and gardens...so much green space! Head out for a walk or run and enjoy the nature side of the city.")  ,

              h5("Not sure if you want to take public transporation yet? There are CitiBikes, easy-to-use pay-by-ride bikes, scattered all around the city to help you get to your next social-distanced hang-out. Use this map to find a bike near you and  bike-friendly roads to travel on.")  ,
              
              h5("Lastly, you can stay up-to-date with Covid-19 cases throughout NYC with this map."),
              h5("While traveling may seem harder nowadays, you can still get out and have fun here in New York City."),

              leafletOutput("nyc_map", width = 1200, height = 800),

              absolutePanel(id = "elements", class = "panel panel-default", top = 325, left = 250, width = 270, height = 270,
                            fixed = TRUE, draggable = TRUE,bottom = "auto",right="auto",
                            tags$p("Add layers:"),
                            checkboxInput("covid", "Covid-19 Rates"),
                            checkboxInput("open_street", "Open Streets"),
                            checkboxInput("parks", "Parks"),
                            checkboxInput("citibike", "Citibike Stations"),
                            checkboxInput("bike_lanes", "Bike Paths"),
                            
                            br(),
                            checkboxInput("directions", "Get Directions"),
                            conditionalPanel(condition = "input.directions == true",
                                             htmlOutput("text2")
                                             
                            ),
                            conditionalPanel(
                              condition = "input.parks == true", 
                              selectInput("parktype", "Park Type:",
                                          list("Park", "Triangle/Plaza", "Parkway", "Recreation Field/Courts", "Playground", "Nature Area"), multiple = F, selected = "Park")
                            ),
                            
                            conditionalPanel(
                              br(),
                              h6(tags$b("See what's open!")),
                              condition = "input.open_street == true",
                              selectizeInput("openstreetday", "Day of Visit:",
                                             list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), selected="Friday")
                            ),
                            conditionalPanel(
                              condition = "input.open_street == true",
                              sliderInput("timesopen", "Time Open (Hour)", 
                                          min = 0, max = 24, value = c(8,12), step = 1)
                            ))),
      tabItem(tabName = "world_data",h2("World data")),
      tabItem(tabName = "policy",h2("Travel policy")),
      tabItem(tabName = "flight",h2("Flight Search"),fluidPage(box(width = 12,
                                                                   div(style="display:inline-block",textInput("Departure","Departure From: ","",'190px',"New York")),
                                                                   div(style="display:inline-block",textInput("Destination","Destination: ","",'190px','Tokyo')),
                                                                   div(style="display:inline-block",textInput("Departure_Date","Departure Date: ","",'190px','yyyy-mm-dd')),
                                                                   div(style="display:inline-block",actionButton("Search",icon("refresh"),label="Search",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                                   verbatimTextOutput("value"),
                                                                   verbatimTextOutput("value2")
      ),
      div(uiOutput("ticket"))
      )),
      tabItem(tabName = "About",h4("Group Member"))
    )
  )
)