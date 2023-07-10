## R Code: mapping the Foundational Infrastructure in Vienna

library(sf) # mittlerweile standard paket für vektor geodaten in R    # spatial formation package
library(osmdata)
library(tidyverse)
library(mapview) # interkative, schnelle karten
library(ggthemes) # pretty ggplots
library(here)
library(glue)
library(jsonlite)
library(hereR)
library(readr)


# data --------------------------------------------------------------------
data_points = read_sf(here("Data/healthcare_final/healthcare_points_final.geojson"))
data_zbez = read_sf(here("Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp"))
data_iso = read_sf(here("Data/Isochrones/Isochrones.geojson"))


# für jeden zbez ----------------------------------------------------------
cents = st_centroid(data_zbez)
isochrones=c(15)
apiKey = "0kOQyCByMkYR55j-46uA1tTpuryTSjnT4lI3vj7TvwY"
set_key(api_key = apiKey)


# uncomment for all
# result = map(1:nrow(cents), function(i){
# test with 10
result = map(1:10, function(i){

  cat(glue(i, "/", 10, "\r"))

  # zbez nr i (zeilennumnner)

  # centroid
  row = cents[i,]


  # zbez
  zbez = data_zbez[i, ]

  # ISOCHRONE
  times = isochrones *60 # time in seconds

  iso = hereR::isoline(        # use hereR package to send API request
    row,
    datetime = Sys.time(),
    range = times,
    range_type = "time",
    transport_mode = "pedestrian"
  )


  # find points in zbez
  # das hier ist eine räumliche intersection!!
  ## beides das gleich!!
  # st_intersection(data_points, zbez)
  healthcare_in_zbez = data_points[zbez, ] %>%
    mutate(
      inZbez = TRUE
    )

  # find points in isochroine
  healthcare_in_isochrone = data_points[iso, ] %>%
    mutate(
      inZbez = FALSE
    )

  #  in isochrone, aber nicht in zbez
  healthcare_in_iso_but_no_zbez = anti_join(healthcare_in_isochrone, healthcare_in_zbez %>% st_drop_geometry(), join_by(name))


  # mapview(healthcare_in_zbez, col.regions="yellow") + mapview(healthcare_in_isochrone, col.regions="green") + mapview(in_iso_but_no_zbez, col.regions="red") +
  #   mapview(zbez) + mapview(iso)

  # in zbez + in isochrone (aber nicht in zbez)
  healthcare_iso_zbez = bind_rows(healthcare_in_zbez, healthcare_in_iso_but_no_zbez)




  # um welchen zbez handelt es sich
  healthcare_iso_zbez[["zbez"]] = zbez[["ZBEZ"]]
  # mapview(healthcare_iso_zbez, zcol="inZbez")

  ######
  ##### WEIGHTS
  ######

  # make long to wide!
  healthcare_iso_zbez %>%
    st_drop_geometry() %>%
    group_by(zbez, inZbez, amenity) %>%
    summarise(
      n_amenities = n()
    ) %>%
    pivot_wider(
      names_from = c("inZbez", "amenity"),
      values_from = "n_amenities"
    ) -> final


  return(final)



})


# alle Zählbezirke zusammenschreiben
all_results = result %>% bind_rows() %>%
  # sortieren nach zbez
  arrange(zbez)

mapview(all_results, zcol="inZbez")


# write to disk
output_path = here("Data/results/all_points_in_zbez_iso.geojson")
# delete old version
unlink(output_path)
write_sf(all_results, output_path)



