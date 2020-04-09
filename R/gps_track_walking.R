library(tidyverse)
library(sf)
library(ggpmthemes)
library(ggtext)
library(osmdata)
library(patchwork)


theme_set(theme_light_modified(base_family = "Roboto Condensed"))

dat <- plotKML::readGPX("~/Downloads/010420.gpx")

df <- bind_rows(dat$tracks[[1]]) %>%
  as_tibble() %>%
  mutate(ele = parse_number(ele)) %>%
  mutate(time = lubridate::parse_date_time(time, orders = "ymdHMS"))

df_sf <- df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

map <- df_sf %>%
  st_bbox() %>%
  opq() %>%
  add_osm_feature("highway") %>%
  osmdata_sf()

df_sf %>%
  ggplot() +
  geom_sf(data = map$osm_lines, size = 0.1, color = "gray75") +
  geom_sf(aes(color = ele), size =  0.25) +
  scale_color_viridis_c() +
  coord_sf() +
  theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  "graphs/gps_track_walking.pdf",
  device = cairo_pdf
)

 max(df$time) - min(df$time)

