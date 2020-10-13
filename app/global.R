#### install and load required r libraries

# list of packages used
pack <- c("tidyverse", "shiny", "sf", "leaflet", "jsonlite", "raster", "tigris", "shinydashboard",
          "httr", "jsonlite", "rlist")

# load packages and data
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pack)

# if not already installed, install and load packages
for (package in pack) {
  pacman::p_load(package, character.only = TRUE, dependence=TRUE)
}

if(!require('openrouteservice')){
  remotes::install_github("GIScience/openrouteservice-r")
  library(openrouteservice)
}

## Load shapefile data
parks <- st_read("../output/nyc_parks/nyc_parks.shp")
citibike<-fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")
citibike <- citibike$data$stations
bike_lanes <- st_read("../data/Bicycle_Routes/geo_export_21a36b1c-263a-41be-a2e0-091316a94ecc.shp")
open_street <- st_read("../output/open_street/open_streets.shp")
covid_df <- read_csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/data-by-modzcta.csv")

data.use <- covid_df %>% dplyr::select(MODIFIED_ZCTA, COVID_CASE_COUNT, NEIGHBORHOOD_NAME)
options(tigris_use_cache = TRUE)
char_zips <- zctas(cb = TRUE, starts_with = "1", state="New York")

nyc_zips <- readLines("../output/nyc_zipcodes.csv")
char_zips <- char_zips %>% filter(ZCTA5CE10 %in% nyc_zips)

data.use$MODIFIED_ZCTA <- as.character(data.use$MODIFIED_ZCTA)
char_zips.use <- geo_join(char_zips,
                          data.use,
                          by_sp = "GEOID10",
                          by_df = "MODIFIED_ZCTA",
                          how = "left")
char_zips.use <- as_Spatial(char_zips.use)


