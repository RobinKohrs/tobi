library(tidyverse)
library(here)
library(glue)
library(sf)
library(rajudas)
library(jsonlite)
library(rvest)


# htmls -------------------------------------------------------------------
htmls = dir("~/Desktop/htmls/", full.names = T)

all_docs = map(seq_along(htmls), function(i){

  html = read_html(htmls[[i]])

  results = html %>% html_elements(".search-result")

  names_and_addresses = map(seq_along(results), function(j){
     res = results[[j]]

     name = res %>% html_element("p.label") %>% html_text()
     address = res %>% html_elements("a > span.icon-text.mr-3 + span")
     if(length(address) == 0){
       address = NA
     }else{
       address = address %>% .[[1]] %>% html_text()
     }

     l = list(
       name = name,
       address = address
     )

  })

  return(names_and_addresses)
})

a =   bind_rows(all_docs)



# geodcode ----------------------------------------------------------------
library(tidygeocoder)
with_address = a %>% distinct() %>% filter(!is.na(address))

coordiantes = with_address %>%
  geocode(address, method="osm", lat = "lat", lon="lon")
