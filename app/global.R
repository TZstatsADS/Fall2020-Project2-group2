#### install and load required r libraries

# list of packages used
pack <- c("tidyverse", "shiny", "sf", "leaflet", "jsonlite", "raster", "tigris", "shinydashboard",
          "httr", "jsonlite", "rlist","shinycssloaders","RCurl","rgdal","shinythemes","tmap","viridis","DT")

#Kristen's part
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

# #--------------------------------------------------------------------
#Elise + Jaival's part
#Create df of country's COVID policies

allchanges<-getURL("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest_allchanges.csv")
allchanges <- read.csv(text = allchanges)

allchanges$CountryName = str_replace_all(allchanges$CountryName,
                                         c('United States'="United States of America",
                                           'Democratic Republic of Congo'='Dem. Rep. Congo',
                                           'Central African Republic'='Central African Rep.',
                                           'Kyrgyz Republic'='Kyrgyzstan',
                                           'Slovak Republic'='Slovakia',
                                           "Cote d'Ivoire" = "Côte d'Ivoire"))

allchanges$Flag = str_replace_all(allchanges$Flag,
                                  c('0'="Regional",'1'='National'))

allchanges = allchanges %>% filter(PolicyValue !='NA') %>% mutate(region = tolower(CountryName),value = PolicyValue)
write.csv(allchanges, '../output/allchanges.csv')

#Create International travel policy var
intl_travel = allchanges%>%filter(PolicyType=='C8: International travel controls') 
intl_travel$value = str_replace_all(intl_travel$value,c('0'='no restrictions','1'='screening arrivals',
                                                        '2'='quarantine arrivals from some/ all regions',
                                                        '3'='ban arrivals from some regions',
                                                        '4'='ban on all regions/total border closure'))

#Latest international travel policy values
latest_index = as.vector(length(intl_travel$region)-match(unique(intl_travel$region),rev(intl_travel$region))+1)
latest_intl_travel = intl_travel[latest_index,]
names(latest_intl_travel)<-c('CountryName','intl_travel-CountryCode','intl_travel-Date',
                             'International Travel','intl_travel-PolicyValue','intl_travel-Flag',	
                             'intl_travel-Notes',	'intl_travel-region',	'intl_travel-value')

#Create var of internal movement policy
dom_move = allchanges%>%filter(PolicyType=='C7: Restrictions on internal movement')
dom_move$value = str_replace_all(dom_move$value,c('0'= 'no measures','1'= 'recommend not to travel between regions',
                                                  '2' = 'restrictions in place'))

#Latest domestic travel policy values
latest_index = as.vector(length(dom_move$region)-match(unique(dom_move$region),rev(dom_move$region))+1)
latest_dom_move = dom_move[latest_index,]
names(latest_dom_move)<-c('CountryName','dom_move-CountryCode','dom_move-Date',
                          'Domestic Movement','dom_move-PolicyValue','dom_move-Flag',	
                          'dom_move-Notes',	'dom_move-region',	'dom_move-value')

#Create var of stay at home order
stayhome = allchanges%>%filter(PolicyType=='C6: Stay at home requirements')
stayhome$value = str_replace_all(stayhome$value,c('0' = 'no measures', '1' = 'recommend not leaving house',
                                                  '2' = 'require not leaving house except for essentials',
                                                  '3' = 'require not leaving house with min exceptions'))
#Latest stay at home policy
latest_index = as.vector(length(stayhome$region)-match(unique(stayhome$region),rev(stayhome$region))+1)
latest_stayhome = stayhome[latest_index,]
names(latest_stayhome)<-c('CountryName','stayhome-CountryCode','stayhome-Date',
                          'Stay Home','stayhome-PolicyValue','stayhome-Flag',	
                          'stayhome-Notes',	'stayhome-region','stayhome-value')

#Create var gathering restriction
gathering = allchanges%>%filter(PolicyType=='C4: Restrictions on gatherings')
gathering$value = str_replace_all(gathering$value,c('0' = 'no restrictions',
                                                    '1' = 'restrictions on gatherings > 1000 people',
                                                    '2' = 'restrictions on gatherings between 101-1000 people',
                                                    '3' = 'restrictions on gatherings between 11-100 people',
                                                    '4' = 'restrictions on gatherings of <= 10 people'))
#Latest gathering restriction policy
latest_index = as.vector(length(gathering$region)-match(unique(gathering$region),rev(gathering$region))+1)
latest_gathering = gathering[latest_index,]
names(latest_gathering)<-c('CountryName','gathering-CountryCode','gathering-Date',
                           'Gathering Restrictions','gathering-PolicyValue','gathering-Flag',	
                           'gathering-Notes',	'gathering-region',	'gathering-value')

#Create var on closing public transport
public_transport = allchanges%>%filter(PolicyType=='C5: Close public transport')
public_transport$value = str_replace_all(public_transport$value,c('0' = 'no measures',
                                                                  '1' = 'recommend closing/ significantly reduce volume',
                                                                  '2' = 'require closing'))
#Latest public trans policy
latest_index = as.vector(length(public_transport$region)-match(unique(public_transport$region),rev(public_transport$region))+1)
latest_public_transport = public_transport[latest_index,]
names(latest_public_transport)[names(latest_public_transport) == "PolicyType"] <- "Public Transport"
names(latest_public_transport)<-c('CountryName','public_transport-CountryCode','public_transport-Date',
                                  'Public Transport','public_transport-PolicyValue','public_transport-Flag',	
                                  'public_transport-Notes',	'public_transport-region','public_transport-value')

write.csv(latest_intl_travel, file = '../output/Latest_Intl_Travel.csv',row.names=F)
write.csv(latest_dom_move, file = '../output/Latest_dom_move.csv',row.names=F)
write.csv(latest_stayhome, file = '../output/Latest_stayhome.csv',row.names=F)
write.csv(latest_gathering, file = '../output/Latest_gathering.csv',row.names=F)
write.csv(latest_public_transport, file = '../output/Latest_public_transport.csv',row.names=F)

latest_intl_travel <- read.csv('../output/Latest_Intl_Travel.csv')
latest_dom_move <- read.csv('../output/Latest_dom_move.csv')
latest_stayhome <- read.csv('../output/Latest_stayhome.csv')
latest_gathering <- read.csv('../output/Latest_gathering.csv')
latest_public_transport <- read.csv('../output/Latest_public_transport.csv')

latest_orig <- merge(latest_intl_travel, latest_dom_move, by= 'CountryName',duplicateGeoms = TRUE)
latest_orig <-merge(latest_orig,latest_stayhome,by= 'CountryName',duplicateGeoms = TRUE)
latest_orig <-merge(latest_orig,latest_gathering,by= 'CountryName',duplicateGeoms = TRUE)
latest_orig <-merge(latest_orig,latest_public_transport,by= 'CountryName',duplicateGeoms = TRUE)

#Download the spatial polygons dataframe in this link
# https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/

output_shapefile_filepath <- "../output/countries_shapeFile.RData"

#if already has countries_shapeFile.RData under output folder, no need to process it again
#otherwise, read files from data folder to create countries_shapeFile.RData under output folder
if(file.exists(output_shapefile_filepath)){
  load(output_shapefile_filepath)
}else{
  countries <- readOGR(dsn ="../data/ne_50m_admin_0_countries",
                       layer = "ne_50m_admin_0_countries",
                       encoding = "utf-8",use_iconv = T,
                       verbose = FALSE)
  save(countries, file=output_shapefile_filepath)
}


