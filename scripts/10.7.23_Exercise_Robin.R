## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) # mittlerweile standard paket für vektor geodaten in R    # spatial formation package
library(osmdata)
library(tidyverse)
library(mapview) # interkative, schnelle karten
library(ggthemes) # pretty ggplots

library(here)
library(glue)
# library(rajudas)
library(jsonlite)
library(hereR)
library(readr)

getwd()

#read data
census_districts_vienna = read_sf("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")   #  census district data from city of Vienna

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
  filter(!is.na(ZBEZ)) |>    # to cut providers not in vienna (they have no ZBEZ data)
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


# get neighborhood isochrones ------------------------------------------------------------

apiKey = "0kOQyCByMkYR55j-46uA1tTpuryTSjnT4lI3vj7TvwY"
#not working: apiKey = readLines(here(".api"))

# urls ------------------------------------------------------
base_here_url = glue("https://isoline.route.ls.hereapi.com/routing/7.2/calculateisoline.json?apiKey={apiKey}")



# get 15min isochrones for census district centroids---------------------------------------------------------------
cents = st_centroid(census_districts_vienna)
isochrones = c(15)

# set api key
hereR::set_key(apiKey)

# setup array for sroting isochrons

isochrone_calcuation = map(1:nrow(cents), function(j){

  cat(glue("{j} / 250"), "\r")

  row = cents[j, ] # j is the row index

  times = isochrones *60 # time in seconds

  res = hereR::isoline(        # use hereR package to send API request
    row,
    datetime = Sys.time(),
    range = times,
    range_type = "time",
    transport_mode = "pedestrian"
  )

  res[["zbez"]] = row$ZBEZNR

  # sleep to not get banned from API
  Sys.sleep(1)

  return(res)

})

View(all_isochrones)
# bind and save isochrone dataset with write_sf(res, path)
all_isochrones = bind_rows(isochrone_calcuation)

write_sf(all_isochrones, "Isochrones.geojson")




# get census district + neighboring data ---------------------------------------------------------------

isochrones_data <- st_read("Isochrones.geojson")
isochrones_data_final <- isochrones_data |>
  rownames_to_column(var = "ZBEZ") |>
  select(ZBEZ)

View(isochrones_data_final)

# get difference between isochrone and zbez

# test
# a = read_sf("~/Desktop/deelte/one.geojson")
# b = read_sf("~/Desktop/deelte/two.geojson")

# ignore world as sphere!
# sf::sf_use_s2(use_s2 = F)

# example for diff between isochrone and zbez
 symDif = st_sym_difference(census_districts_vienna$geometry[1], isochrones_data_final$geometry[1])

View(census_districts_vienna)
View(isochrones_data_final)
mapview(symDif)

#get neighboring areas of each census district (centroids)


neighboring_areas = map(1:nrow(census_districts_vienna), function(j){
  symDif = st_sym_difference(isochrones_data_final$geometry[j], census_districts_vienna$geometry[j])
})


mapview(neighboring_areas[3])    # but not ordered from zbez 1 to 250?

#get healthcare_points that insect with neighboring areas

# replicate: census_districts_vienna$hospitals <- lengths(st_intersects(census_districts_vienna, hospitals_official_clean))

View(census_districts_vienna)
View(neighboring_areas)
View(hospitals_official_clean)
mapview(hospitals_official_clean)

healthcare_points_neighboring$hospitals <- lengths(st_intersects(neighboring_areas, hospitals_official_clean))


#what arguments do st_intersects need and what is the output like?
# input: polygon, and data set geometry points -> output: one list?






#robin's solution but i do not understand?

# for each cutted isochrone find all the points within ------------------------
intersecting_points_for_each_isochrone = map(1:nrow(all_isochrones), function(rowIndex){

  row = all_isochrones[rowIndex, ]

  # make it bigger -> dont do that
  # delete that
  fake_polygon = st_buffer(row, 3000)

  # find all the points (service provider in real life)
  #
  intersecting_points = cents[fake_polygon, ]
  intersecting_points[["isochrone_id"]] = rowIndex

  return(intersecting_points)

})






















































# calculate isochrones ----------------------------------------------------
isochrones = c(15)


# set api key
hereR::set_key(apiKey)

# setup array for routing isochrones

results = map(1:nrow(census_districts_vienna_final$centroids), function(j){

  cat(glue("{j} / 250"), "\r")

  # j is the row index
  row = census_districts_vienna_final$centroids[j, ]

  # send request to api -----------------------------------------------------

  # time in seconds
  times = isochrones *60

  # use hereR package
  res = hereR::isoline(
    row,
    datetime = Sys.time(),
    range = times,
    range_type = "time",
    transport_mode = "pedestrian"
  )

  str(res)
  res[["census_districts_vienna_final$centroids"]] = row$ZBEZNR

  # sleep to not get banned from API
  Sys.sleep(1)

  ## save to disk
  # write_sf(res, path)

  return(res)

})


# bind all rows together --------------------------------------------------
all_isochrones = bind_rows(results)

View(all)

# save all isochrones
write_sf(all_isochrones, "~/Desktop/test.geojson")
# fake points (centroids) -------------------------------------------------------------



# create difference between isochrone and zbez ----------------------------

# test
# a = read_sf("~/Desktop/deelte/one.geojson")
# b = read_sf("~/Desktop/deelte/two.geojson")

# ignore world as sphere!
# sf::sf_use_s2(use_s2 = F)

# example for diff between isochrone and zbez
# symDif = st_sym_difference(a,b)




# for each cutted isochrone find all the points within ------------------------
intersecting_points_for_each_isochrone = map(1:nrow(all_isochrones), function(rowIndex){

  row = all_isochrones[rowIndex, ]

  # make it bigger -> dont do that
  # delete that
  fake_polygon = st_buffer(row, 3000)

  # find all the points (service provider in real life)
  #
  intersecting_points = cents[fake_polygon, ]
  intersecting_points[["isochrone_id"]] = rowIndex

  return(intersecting_points)

})









