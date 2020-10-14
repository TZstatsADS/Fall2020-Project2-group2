
library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(rlist)


library(shinycssloaders)
library(rintrojs)
library(shinyBS)
library(shinyjs)



ui <- dashboardPage(
  skin="purple",
  dashboardHeader(title = "Traveling During Covid-19", titleWidth="280"),
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
                valueBoxOutput('covid_case_world'), 
                valueBoxOutput('covid_case_nyc'), 
                valueBoxOutput('countries_without_travel_bans')
              ), 
              box(width = 15, img(src = 'https://scontent-lga3-2.xx.fbcdn.net/v/t31.0-0/p417x417/23213298_128755171176807_7649637594963973686_o.jpg?_nc_cat=104&_nc_sid=e3f864&_nc_ohc=xnJ3SuXGIT4AX_lpUbR&_nc_ht=scontent-lga3-2.xx&tp=6&oh=788f64b8e7cc1513acab12b99b5ce96e&oe=5FAD9C48',
                                  width = '100%', height = '60%')),
              h3("Where New Yorkers Can Travel During Covid-19"),
              fluidRow(box(width = 12, title = "Introduction", status = "primary",
                           solidHeader = TRUE, 
                           h5("Since China reported its first cases to the World Health Organization (WHO) in December, authorities in 214 countries have reported about 37 million Covid-19 cases and 1.1 million deaths. "), 
                           h5('It can seem like with Covid-19, it is impossible to travel. This app was built for New Yorkers, to show them all the places they are still able to travel to or safely hang-out at, even with the pandemic still present.')
              )), 
              useShinyjs(),
              introjsUI(),
              fluidRow(
                column(
                  width = 12,
                  bsButton(inputId = "World_covid_case", 
                           label = "WORLD", 
                           icon = icon("globe"), 
                           style = "primary", 
                           size = 'large', type = 'action'),
                  bsButton(inputId =  "NYC_covid_case", 
                           label = "NYC", 
                           icon = icon("city", class = "font-awesome"), 
                           style = "primary", 
                           size = 'large', type = 'action')
                  
                )
              ), 
              fluidRow(
                div(
                  id = "world_panel",
                  column(
                    width = 12,
                    uiOutput("panel1")
                  ))
                
              ), 
              fluidRow(
                div(
                  id = "nyc_panel",
                  column(
                    width = 12,
                    uiOutput("panel2")
                  ))
                
              )
              
              
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
                                          choices = intl_travel_legend)),
                
                absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              top = 50, left = 440, right = "auto", bottom = "auto", width = 180, height = 'auto',
                              selectInput(inputId='dom_move',label = 'Domestic Movement',
                                          choices = dom_move_legend)),
                absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              top = 50, left = 640, right = "auto", bottom = "auto", width = 180, height = "auto",
                              selectInput(inputId='stayhome',label = 'Stay at Home',
                                          choices = stayhome_legend)),
                absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              top = 50, left = 840, right = "auto", bottom = "auto", width = 180, height = "auto",
                              selectInput(inputId='gathering',label = 'Gathering Restrictions',
                                          choices = gathering_legend)),
                absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              top = 50, left = 1040, right = "auto", bottom = "auto", width = 180, height = "auto",
                              selectInput(inputId='pub_trans',label = 'Public Transport',
                                          choices = public_transport_legend)),
                absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE, draggable = FALSE,
                              top = 50, left = 1240, right = "auto", bottom = "auto", width = 180, height = "auto",
                              selectInput('covidcases',label = 'Current Cases',
                                          choices = list('Daily Count','Percentage Change in MA for the last 2 weeks'))),
                
                #fluidRow(column(7,
                #sidebarLayout(
                #max-height: 400px;
                #sidebarPanel(id = "tPanel",style = "overflow-y:scroll; position:relative;",
                absolutePanel(id = "control", class = "panel panel-default", fixed = FALSE, draggable = TRUE,
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
                                                                       'Stay at Home','Gathering Restrictions','Public Transport','Current Cases'))),
                              
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
                            div(style="display:inline-block",textInput("Departure_Date","Departure Date: ",Sys.Date()+14,'190px','yyyy-mm-dd')),
                            div(style="display:inline-block",actionButton("Search",icon("refresh"),label="Search",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                            verbatimTextOutput("value"),
                            verbatimTextOutput("value2")
                                                                  
      ),
      div(uiOutput("ticket"))
      )),
tabItem(tabName = "About",
        box(title = 'Contributors', width = 12, status = "primary",
            solidHeader = TRUE, 'Kristen Akey, Jaival Desai, Ziyu Kong, Yiran Lin, Linh Nguyen'
        ), 
        box(width = 12, title = "Data Sources", status = "primary",
            solidHeader = TRUE, "— World-wide Policies: ",
            tags$a(href = "https://github.com/OxCGRT/covid-policy-tracker/tree/master/data", 
                   "Latest changes"),
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
            
            
        ),
        box(width = 12, title = "Project Code",status = "primary",
            solidHeader = TRUE, "All code for this project is on",
            tags$a(href = "https://github.com/TZstatsADS/Fall2020-Project2-group2",
                   "Github"), ".")
)
    )
  )
)