library(tidyverse)
library(here)
library(glue)
library(sf)
library(mapview)
library(rajudas)
library(jsonlite)
library(hereR)


# load api key ------------------------------------------------------------
apiKey = readLines(here(".env"))

# paths -------------------------------------------------------------------
path_zaehlbezirk = here("data_raw/Data/Census Districts/ZAEHLBEZIRKOGDPolygon.shp")

# urls ------------------------------------------------------
base_here_url = glue("https://isoline.route.ls.hereapi.com/routing/7.2/calculateisoline.json?apiKey={apiKey}")


# load data ---------------------------------------------------------------
zbez = read_sf(path_zaehlbezirk)


# centroids ---------------------------------------------------------------
cents = st_centroid(zbez)

# plot
mapview(zbez) + mapview(cents)


# calculate isochrones ----------------------------------------------------
isochrones = c(15)

# set api key
hereR::set_key(apiKey)

# setup array for sroting isochrons

results = map(1:nrow(cents), function(j){

    cat(glue("{j} / 250"), "\r")

    # j is the row index
    row = cents[j, ]

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

    res[["zbez"]] = row$ZBEZNR

    # sleep to not get banned from API
    Sys.sleep(1)

    ## save to disk
    # write_sf(res, path)

    return(res)

})


# bind all rows together --------------------------------------------------
all_isochrones = bind_rows(results)

# save all isochrones
write_sf(all_isochrones, "~/Desktop/test.geojson")
# fake points (centroids) -------------------------------------------------------------

# for each isochrone find all the points within
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


# plot --------------------------------------------------------------------
df_all_points = bind_rows(intersecting_points_for_each_isochrone)
mapview(df_all_points, zcol="isochrone_id") + mapview(all_isochrones)


# symetrical difference in R ----------------------------------------------
a = read_sf("~/Desktop/deelte/one.geojson")
b = read_sf("~/Desktop/deelte/two.geojson")

# ignore world as sphere!
sf::sf_use_s2(use_s2 = F)

# example for diff between isochrone and zbez
symDif = st_sym_difference(a,b)













