library(tidyverse)
library(sf)
library(ggfx)
library(osmdata)

permits <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/sf_permits.csv"
  )

permits <- permits |>
  drop_na(location) |>
  st_as_sf(wkt = "location", crs = 4326)

sf_shapefile <- curl::curl_download(
  "https://data.sfgov.org/api/geospatial/pty2-tcw4?method=export&format=Shapefile",
  destfile = tempfile(fileext = ".zip")
)

san_francisco <- st_read(paste0("/vsizip/", sf_shapefile))

crsuggest::suggest_crs(san_francisco)

san_francisco <- san_francisco |>
  st_transform(7132)

permits <- permits |>
  st_transform(7132)

permits

permits |>
  as_tibble() |>
  count(year = lubridate::year(permit_creation_date))

permits |>
  count(permit_type_definition)

# Plot --------------------------------------------------------------------

outline <- san_francisco |>
  st_simplify() %>%
  st_union() %>%
  st_buffer(dist = 750)

p <- san_francisco |>
  mutate(counts = lengths(st_intersects(x = san_francisco, permits))) |>
  ggplot() +
  geom_sf(aes(fill = counts), size = 0.25, color = "grey95") +
  geom_sf_text(
    data = st_centroid(san_francisco),
    aes(label = str_wrap(name, 10)),
    size = 1.75,
    color = "#3c3c3c",
    check_overlap = TRUE,
    family = "Open Sans"
  ) +
  paletteer::scale_fill_paletteer_c(
    "gameofthrones::lannister",
    direction = -1,
    breaks = scales::breaks_pretty(n = 8),
    guide = guide_colorbar(
      barwidth = unit(16, "cm"),
      barheight = unit(0.25, "cm"),
      ticks.linewidth = unit(1, "cm"),
      title.position = "top",
      title = "Total number of permits",
      title.hjust = 0.5
    )
  ) +
  with_shadow(
    geom_sf(
      data = outline,
      color = "white",
      fill = NA,
      size = 0.75
    ),
    sigma = 4,
    colour = "orange",
    x_offset = 2,
    y_offset = 2
  ) +
  labs(
    title = glue::glue(
      "Construction permits delivered between\n{paste(range(lubridate::year(permits$permit_creation_date)), collapse = ' and ')} in San Francisco"
    ),
    caption = "Data from https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-07-05 | @philmassicotte"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat", color = "white"),
    plot.background = element_rect(fill = "#3c3c3c", color = NA),
    panel.background = element_rect(fill = "#3c3c3c", color = NA),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(color = "grey50"),
    axis.title = element_blank(),
    legend.position = "top",
    plot.title = element_text(
      size = 28,
      hjust = 0.5,
      family = "Baloo 2",
      face = "bold"
    ),
    plot.subtitle = element_text(size = 9, family = "IBM Plex Sans Light"),
    plot.title.position = "plot",
    plot.caption = element_text(size = 8, family = "IBM Plex Sans Light", color = "grey75")
  )

ggsave(
  here::here("graphs", "008_construction_permits_san_francisco.png"),
  dpi = 300,
  width = 8,
  height = 8
)
