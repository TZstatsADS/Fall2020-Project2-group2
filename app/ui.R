
library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(rlist)
ui <- dashboardPage(
  skin="purple",
  dashboardHeader(title = strong("Traveling During Covid-19"), titleWidth="300px"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home",icon = icon("home")),
      menuItem("World",tabName = "World",menuSubItem("Travel Policy",tabName = "policy",icon=icon("route","font-awesome")),menuSubItem("Flights",tabName="flight",icon=icon("plane-departure","font-awesome")),icon = icon("globe",lib = "font-awesome")),
      menuItem("New York City", tabName = "New York City",menuSubItem("Travel NYC",tabName = "nyc_data",icon=icon("chart-line","font-awesome")),icon = icon("city","font-awesome")),
      menuItem("About",tabName="About",icon=icon("question-circle","font-awesome"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",       
      fluidRow(
        box(width = 15, 
            solidHeader = TRUE, h1("Traveling During Covid-19"),
            h5("Since China reported its first cases to the World Health Organization (WHO) in December, authorities in 214 countries have reported about 37 million Covid-19 cases and 1.1 million deaths."),
            h5("It can seem like with Covid-19, it is impossible to travel. This app was built for New Yorkers, to show them all the places they are still able to travel to or safely hang-out at, even with the pandemic still present."),
            )),
        fluidRow(box(width = 15, 
                     solidHeader = TRUE, h3("Overview of the App"),
                     tags$div(tags$ul(
                       tags$li("World: This tab shows each country’s most recent Covid-19 cases and related policies across the world so you can figure out where you can travel during the pandemic. When you’ve chosen a country you can and want to travel to, find the best priced flight with real-time flight information, including departure time and fare price, to finalize your next international voyage."),
                       tags$li("Maps: Even if you can’t travel internationally right now, you can still travel, explore, and hang out around New York City. This tab contains a map of New York City that allows you to find your nearest park or preferred outdoor space, social-distance spots and dining, bike stations, bike lanes, as well as stay up-to-date on Covid-19 cases in New York City."),
                     )))),
      fluidRow(box(width = 15, 
                   solidHeader = TRUE, h3("Data Overview"),
                   h5("World Data"),
                   tags$div(tags$ul(
                     tags$li("Policies: continuously updated latest government policies pulled from Oxford Covid-19 Government Response Tracker."),
                     tags$li("Filter by 1 criteria: heatmap of every country’s policies/ Covid-19 data with detailed notes on policy of interest. Users can also filter out countries that adopt a certain policy (e.g. no measure) within each criteria (e.g. International Travel) by selecting that policy from the dropdown."),
                     tags$li("Filter by Multiple criteria: After selecting values for each and every criteria of interest, the map will highlight those that meet the requirements."),
                     tags$li("Filter by Countries: Select countries of interest. Hover over each country for a list of all its policies and Covid-19 data.")
                   )),
                   h5("New York City Data"),
                   tags$div(tags$ul(
                     tags$li("Open Street data: streets closed off to cars in NYC, for pedistriations to hang-out and restaurants to create outdoor dining during Covid-19."),
                     tags$li("Citibike: shows every Citibike, easy-to-use pay-by-ride bikes, station in New York City."),
                     tags$li("Parks: New York City parks."),
                     tags$li("Directions: find the fastest walking path to get from where you are in the city to where you want to go."),
                     tags$li("Covid-19: up-to-date information of Covid-19 cases in each zip code in New York City.")
                   ))
                   
                   
                   )),
        fluidRow(box(width = 15, solidHeader = TRUE, h4("Contributors"),
                 h5("Kristen Akey, Jaival Desai, Ziyu Kong, Yiran Lin, Linh Nguyen, Siyuan Zhou")
                 ))
      ),
# Kristen's part      
      tabItem(tabName = "nyc_data",
              titlePanel("Where Can I Travel or Hang Out (Social Distance Style) in New York City?"),
              h5("Even if you are unable to travel internationally right now, there are still many things to do safely in New York City."),
              h5("New Yorkers have access to Open Streets, which includes over 100 miles of streets closed off to cars to allow for greater social distancing during Covid-19. Explore and locate the nearest Open Street near you to meet up with your friends for a spacious chat or outdoor dining. New Yorkers also have access to over 100 different parks, recreational centers, and gardens...so much green space! Head out for a walk or run and enjoy the nature side of the city.")  ,

              h5("Not sure if you want to take public transporation yet? There are CitiBikes, easy-to-use pay-by-ride bikes, scattered all around the city to help you get to your next social-distanced hang-out. Use this map to find a bike near you and  bike-friendly roads to travel on.")  ,
              
              h5("Lastly, you can stay up-to-date with Covid-19 cases throughout NYC with this map."),
              h5("While traveling may seem harder nowadays, you can still get out and have fun here in New York City."),

              leafletOutput("nyc_map", width = 1200, height = 800)%>% withSpinner(type=4),

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
      
      
#Elise + Jaival's part
      tabItem(tabName = "policy",h2("Travel policy"),
      fluidPage(
                leafletOutput("map", width = "85%", height = "900")%>% withSpinner(type=4),
                absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              top = 50, left = 240, right = "auto", bottom = "auto", width = 180, height = 'auto',
                              selectInput(inputId='intl_travel',label = 'International Travel',
                                          choices = list('All','no restrictions','screening arrivals',
                                                         'quarantine arrivals from some/ all regions',
                                                         'ban arrivals from some regions',
                                                         'ban on all regions/total border closure'))),
                
                absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              top = 50, left = 440, right = "auto", bottom = "auto", width = 180, height = 'auto',
                              selectInput(inputId='dom_move',label = 'Domestic Movement',
                                          choices = list('All', 'no measures','recommend not to travel between regions',
                                                         'restrictions in place'))),
                absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              top = 50, left = 640, right = "auto", bottom = "auto", width = 180, height = "auto",
                              selectInput(inputId='stayhome',label = 'Stay at Home',
                                          choices = list('All','no measures', 'recommend not leaving house',
                                                         'require not leaving house except for essentials',
                                                         'require not leaving house with min exceptions'))),
                absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              top = 50, left = 840, right = "auto", bottom = "auto", width = 180, height = "auto",
                              selectInput(inputId='gathering',label = 'Gathering Restrictions',
                                          choices = list('All', 'no restrictions',
                                                         'restrictions on gatherings > 1000 people',
                                                         'restrictions on gatherings between 101-1000 people',
                                                         'restrictions on gatherings between 11-100 people',
                                                         'restrictions on gatherings of <= 10 people'))),
                absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              top = 50, left = 1040, right = "auto", bottom = "auto", width = 180, height = "auto",
                              selectInput(inputId='pub_trans',label = 'Public Transport',
                                          choices = list('All','no measures',
                                                         'recommend closing/ significantly reduce volume',
                                                         'require closing'))),
                
                #fluidRow(column(7,
                #sidebarLayout(
                #max-height: 400px;
                #sidebarPanel(id = "tPanel",style = "overflow-y:scroll; position:relative;",
                absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              top = 300, left = 1070, right = "auto", bottom = "auto", width = 180, height = "auto",
                              a("Select a filter below for map"),
                              selectInput(inputId= 'filter',label = 'Filter by',
                                          choices = c('Countries', '1 criterion','Multiple criteria'),selected= '1 criterion'),
                              conditionalPanel(condition = "input.filter == 'Countries'", 
                                               selectInput(inputId= 'Countries', label = "Countries",
                                                           choices = latest_orig$CountryName, selected = "United States of America",
                                                           selectize = TRUE, multiple = TRUE)),
                              
                              conditionalPanel(condition = "input.filter == '1 criterion'", 
                                               selectInput(inputId= 'heatmap', label = "Policy Type",
                                                           choices = c('International Travel','Domestic Movement',
                                                                       'Stay at Home','Gathering Restrictions','Public Transport'))),
                              
                              conditionalPanel(condition = "input.filter == 'Multiple criteria'",
                                               selectInput(inputId= 'choices', label = "Policy Type",
                                                           choices = list('International Travel','Domestic Movement',
                                                                          'Stay at Home','Gathering Restrictions','Public Transport'),
                                                           selectize = TRUE, multiple = TRUE)),
                              style = "opacity: 1"))),
#Sophie's part
      
      tabItem(tabName = "flight",h2("Flight Search"),a("Click here for US citizens travel advisories",href="https://travel.state.gov/content/travel/en/traveladvisories/COVID-19-Country-Specific-Information.html"),

              fluidPage(box(width = 12, div(style="display:inline-block",textInput("Departure","Departure From: ","New York",'190px',"New York")),
                            div(style="display:inline-block",textInput("Destination","Destination: ","Boston",'190px','Boston')),
                            div(style="display:inline-block",textInput("Departure_Date","Departure Date: ","",'190px','yyyy-mm-dd')),
                            div(style="display:inline-block",actionButton("Search",icon("refresh"),label="Search",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                            verbatimTextOutput("value"),
                            verbatimTextOutput("value2")
                                                                  
      ),
      div(uiOutput("ticket"))
      )),
      tabItem(tabName = "About",
              # h4("Data Sources")
              fluidRow(box(width = 15, title = "Data Sources", 
                           solidHeader = TRUE, "— World-wide Policies: ",
                           tags$a(href = "https://github.com/OxCGRT/covid-policy-tracker/tree/master/data", 
                                  "Latest changes"),
                        br(), "—",
                         tags$a(href = "https://github.com/CSSEGISandData/COVID-19", 
                                "World - Coronavirus Data"),
                       br(), "—",
                       tags$a(href = "https://github.com/CSSEGISandData/COVID-19", 
                              "World - Coronavirus Data"),
                       br(), "—",
                       tags$a(href = "https://github.com/nychealth/coronavirus-data", 
                              "New York City Health - Coronavirus Data"),
                       br(), "—",
                       tags$a(href = "https://data.cityofnewyork.us/Health/Open-Streets-Locations/uiay-nctu", 
                              "NYC Open Data - Open Street Locations"),
                       br(), "—",
                       tags$a(href = "https://data.cityofnewyork.us/Recreation/Open-Space-Parks-/g84h-jbjm", 
                              "NYC Open Data - New York City Parks"),
                       br(), "—",
                       tags$a(href = "https://data.cityofnewyork.us/Transportation/Bicycle-Routes/7vsa-caz7", 
                              "NYC Open Data - New York City Bike Lanes"),
                       br(), "—",
                       tags$a(href = "http://gbfs.citibikenyc.com/gbfs/gbfs.json", 
                              "Citibike Station data"),
                       
                       
                       )),
              fluidRow(box(width = 15, h4("Project Code"),
                           solidHeader = TRUE, "All code for this project is on",
                           tags$a(href = "https://github.com/TZstatsADS/Fall2020-Project2-group2",
                                  "Github"), "."))
              
              )
    )
  )
)