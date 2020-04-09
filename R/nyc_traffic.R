library(tidyverse)
library(data.table)
library(dtplyr)
library(vroom)
library(sf)
library(ggpmthemes)
library(ggtext)
library(osmdata)

df <- fread("~/Downloads/DOT_Traffic_Speeds_NBE.csv")

df2 <- unique(df, by = "ID")

df3 <- df[ID == 157, ] %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  separate_rows(link_points, sep = " ") %>%
  separate(
    link_points,
    into = c("lat", "long"),
    sep = ",",
    remove = FALSE
  ) %>%
  mutate_at(vars(c("lat", "long")), parse_number)

df3 %>%
  mutate(data_as_of = as.Date(data_as_of, "%m/%d/%Y")) %>%
  mutate(year = lubridate::year(data_as_of)) %>%
  mutate(month = lubridate::month(data_as_of)) %>%
  group_by(data_as_of, year, month) %>%
  summarise(mean_speed = mean(speed, na.rm = TRUE)) %>%
  filter(year %in% c(2019, 2020)) %>%
  ggplot(aes(x = data_as_of, y = mean_speed, group = factor(year))) +
  geom_line() +
  facet_grid(month~year, scales = "free")

# https://data.cityofnewyork.us/Transportation/DOT-Traffic-Speeds-NBE/i4gi-tjb9

# df <- vroom("~/Downloads/DOT_Traffic_Speeds_NBE.csv", altrep = TRUE) %>%
#   janitor::clean_names()
#
# loc <- df %>%
#   distinct(id, .keep_all = TRUE) %>%
#   separate_rows(link_points, sep = " ") %>%
#   separate(link_points, into = c("lat", "long"), sep = ",", remove = FALSE) %>%
#   mutate_at(vars(c("lat", "long")), parse_number)
#
# write_csv(loc, "~/Desktop/nyc_localisations_traffic.csv")

loc <- read_csv("~/Desktop/nyc_localisations_traffic.csv")

loc %>%
  filter(long < -60) %>%
  ggplot(aes(x = long, y = lat, label = id)) +
  geom_point() +
  coord_map()

loc_sf <- loc %>%
  filter(between(long, -74.5, -73.5)) %>%
  drop_na(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

loc_sf %>%
  ggplot() +
  geom_sf() +
  geom_sf_text(data = sample_n(loc_sf, 100), aes(label = id))

map <- st_read("~/Downloads/nyu_2451_34490/nyu_2451_34490.shp")

map %>%
  ggplot() +
  geom_sf(size = 0.1, color = "gray75") +
  geom_sf(data = loc_sf %>% filter(id == 157), size =  0.25) +
  scale_color_viridis_c() +
  coord_sf() +
  theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  )
