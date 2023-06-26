## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) # mittlerweile standard paket für vektor geodaten in R    # spatial formation package 
library(osmdata)
library(tidyverse)
library(mapview) # interkative, schnelle karten
library(ggthemes)


#read data
census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")  #  census district data from city of Vienna

hospitals_official = read_sf("Data/Hospitals/KRANKENHAUSOGDPoint.shp")  # official data on hospitals from Vienna
pharmacy_official = read_sf("Data/Pharmacy/APOTHEKEOGDPoint.shp")   #official data on pharmacies from Vienna
hospitals_official_clean <- hospitals_official |>
  select("name" = BEZEICHNUN)  |> 
  mutate(amenity = "hospital") 

pharmacy_official_clean <- pharmacy_official |> 
  select("name" = BEZEICHNUN) |>
  mutate(amenity = "pharmacy") 

#get doctors data from osm since no official data
# overpass query = API (application programming interface) von OSM
vienna_bb = opq("austria vienna")   # bb bounding box 

doctors_osm = vienna_bb |>
  osmdata::add_osm_feature(key="amenity", value="doctors") |>
  osmdata_sf()  

#checking if same projectation of different data
#census_test <- census_districts_vienna |> st_transform(census_districts_vienna, crs = st_crs(doctors_osm)) 

doctors_osm = doctors_osm$osm_points  # get osm data points, no polygons available
doctors_osm_clean <- doctors_osm |>
  filter(!is.na(name)) |>
  mutate(amenity = "doctor") |>
  select(name, amenity, geometry)


# data merge  --------------------------------------------------------------

healthcare_data_merged <- bind_rows(hospitals_official_clean, pharmacy_official_clean, doctors_osm_clean) |>
    select(name, amenity, geometry) 
View(healthcare_data_merged)

# data preparation for visualization --------------------------------------

#healthcare points 
healthcare_data_final <- st_join(healthcare_data_merged, census_districts_vienna, join = st_within) |>   
  filter(!is.na(ZBEZ)) |>  # # to cut providers not in vienna (they have no ZBEZ data)
  select(name, amenity, ZBEZ, geometry) |>
  group_by(ZBEZ) 

# census district data
census_districts_vienna$hospitals <- lengths(st_intersects(census_districts_vienna, hospitals_official_clean))

census_districts_vienna$pharmacies <- lengths(st_intersects(census_districts_vienna, pharmacy_official_clean))

census_districts_vienna$doctors <- lengths(st_intersects(census_districts_vienna, doctors_osm_clean))

census_districts_vienna_final <- census_districts_vienna |>
  select(ZBEZ, geometry, hospitals, pharmacies, doctors) |>
  group_by(ZBEZ, geometry) |> 
  mutate(FAI = hospitals * 20 + pharmacies *2 + doctors * 1, # how to weight hospitals pharmacies and doctors???
         centroids = st_centroid(geometry),    # centroids of census districts to calculate distances
        ZBEZ = as.integer(ZBEZ)) |>
  arrange(ZBEZ)

View(census_districts_vienna_final)


# plotting with ggplot2 -------------------------------------------------------

ggplot() +
  geom_sf(data = census_districts_vienna_final, aes(fill = FAI),
          color = "grey20",
          size = 0.15) +
  geom_sf(data = healthcare_data_final, 
          aes(shape = amenity), 
          size = 0.4) +
  
  theme_map() +
  scale_fill_continuous(type = "viridis",
                        direction = -1) +
  scale_shape_discrete() + 
  
  theme(legend.position = "left")



# open question: gewichtugn der neigborhoods reinbringe

# option 1: distanzen von district centroid zu amenties 

census_districts_vienna_final


# luftlinien
dists = st_distance(census_districts_vienna_final$centroids[1], census_districts_vienna_final$centroids[2])
dists

# Routing APIs um "echten" Weg und Dauer zu ermitteln
# Nomanitim von OSM (vermutlich keine Öffis)
# HERE API (https://developer.here.com/documentation/routing-api/dev_guide/index.html) hat Öffis
#   -> 500000


# macht routing sinn wenn ich mit socioeconomic status vergleichen will?
# anstelle von neighboring census districts könnte man 1km fußweg von census district als neighborhood definieren

# what do I want? request direction data from census district centroid that shows a polygon of a reachable radius

#or maybe get 15min reachable isochrone per census district and include all amenities to FAI? 




# routing with here

install.packages("httr")
install.packages("jsonlite")

library(httr)    # to get data from the API from webpage format
library(jsonlite)   # for passing results into jsonfile

library(httr)
library(jsonlite)

# Define the API endpoint URL
url <- "https://xyz.api.here.com/hub/spaces/20138133/search"

# Set the API key in the headers
headers <- c("Authorization" = "Bearer 0kOQyCByMkYR55j-46uA1tTpuryTSjnT4lI3vj7TvwY")

# Set the query parameters
params <- list(q = "Vienna")  # Search for data in Vienna

# Make the GET request
response <- GET(url, headers = headers, query = params)

str(response)
response$content

View(census_districts_vienna_final)
healthcare_data$geometry

test <- httr::GET("https://router.hereapi.com/v8/routes?transportMode=car&origin=48.17835,16.4172&destination=48.23744, 16.38916&return=summary&apikey={0kOQyCByMkYR55j-46uA1tTpuryTSjnT4lI3vj7TvwY}"

str(test)
