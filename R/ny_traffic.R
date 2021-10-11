library(tidyverse)
library(tidyjson)
library(sf)

url <- "https://data.cityofnewyork.us/resource/i4gi-tjb9.geojson?$$app_token=R7TooE2QgjDj1ZK2UkyaSA557"

df <- jsonlite::fromJSON(
  url,
  simplifyDataFrame = TRUE
)

df <- sf::st_read(url)

class(df)

df %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf()
