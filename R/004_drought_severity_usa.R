library(tidyverse)
library(sf)
library(rnaturalearth)
library(pins)

url <-
  pin(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv"
  )

df <- readr::read_csv(url) |>
  janitor::clean_names()

df <- df |>
  mutate(year = lubridate::year(date)) |>
  mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE))

df

df_viz <- df |>
  filter(between(year, 2012, 2021)) |>
  group_by(state, year, month) |>
  summarise(average_dsci = mean(dsci, na.rm = TRUE)) |>
  ungroup()

df_viz


states <-
  ne_states(
    country = "united states of america",
    returnclass = "sf"
  ) |>
  filter(!(name_en %in% c("Alaska", "Hawaii"))) |>
  st_transform(crs = 5931) |>
  st_make_valid() |>
  mutate(state = str_remove(iso_3166_2, "US-"))

outline <- st_union(states)

df_viz <- df_viz |>
  inner_join(states)

df_viz

# Plot --------------------------------------------------------------------

p <- ggplot() +
  geom_sf(
    data = df_viz,
    aes(geometry = geometry, fill = average_dsci),
    color = NA
  ) +
  geom_sf(data = outline, fill = NA, size = 0.1) +
  paletteer::scale_fill_paletteer_c(
    "grDevices::YlOrBr",
    direction = -1,
    breaks = seq(0, 500, by = 100),
    limits = c(0, 500),
    guide = guide_colorbar(title.position = "top")
  ) +
  labs(
    subtitle = str_wrap(
      "The Drought Severity and Coverage Index is an experimental method for converting drought levels from the U.S. Drought Monitor map to a single value for an area. DSCI values are part of the U.S. Drought Monitor data tables. Possible values of the DSCI are from 0 to 500. Zero means that none of the area is abnormally dry or in drought, and 500 means that all of the area is in D4, exceptional drought.",
      120
    ),
    fill = "Drought severity coverage index (DSCI)",
    caption = "Data from https://droughtmonitor.unl.edu/Data.aspx | @philmassicotte"
  ) +
  facet_grid(year ~ month) +
  theme_void(base_family = "Montserrat") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.85, 1.1),
    legend.key.width = unit(3, "line"),
    legend.key.height = unit(0.5, "line"),
    legend.title.align = 0.5,
    legend.title = element_text(
      family = "Montserrat",
      size = 8,
      color = "grey50"
    ),
    legend.text = element_text(
      family = "Montserrat",
      size = 8,
      color = "grey50"
    ),
    legend.direction = "horizontal",
    plot.margin = margin(30, 20, 0, 20),
    strip.text = element_text(
      margin = margin(0, 0, 2, 0),
      color = "grey40",
      size = 10
    ),
    plot.subtitle = element_text(
      margin = margin(3, 0, 20, 0),
      size = 8,
      family = "Heebo",
      lineheight = 1.5
    ),
    plot.caption = element_text(colour = "grey60", size = 8)
  )

file <- here::here("graphs", "004_drought_severity_usa.png")

ggsave(
  file,
  dpi = 300,
  width = 12,
  height = 8
)

# knitr::plot_crop(file)
