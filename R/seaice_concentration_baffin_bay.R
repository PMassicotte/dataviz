library(tidyverse)
library(sf)
library(terra)
library(pbmcapply)
library(rcartocolor)
library(httr2)
library(jsonlite)

library(ggpmthemes)

theme_set(theme_montserrat())

theme_update(
  panel.border = element_blank(),
  panel.background = element_rect(fill = "#2B2D42", color = NA),
  panel.grid = element_line(color = "gray35", size = 0.1),
  plot.background = element_rect(fill = "#2B2D42", color = NA),
  plot.caption = element_text(colour = "grey75", size = 8),
  legend.background = element_rect(fill = "#2B2D42"),
  legend.key = element_rect(fill = "#2B2D42"),
  legend.text = element_text(color = "white"),
  axis.ticks = element_blank(),
  axis.title = element_text(color = "white"),
  axis.text = element_text(color = "white")
)

url <- "https://geo.vliz.be/geoserver/wfs?request=getfeature&service=wfs&version=1.1.0&typename=MarineRegions:iho&outputformat=json&filter=%3CPropertyIsEqualTo%3E%3CPropertyName%3Eid%3C%2FPropertyName%3E%3CLiteral%3E14A%3C%2FLiteral%3E%3C%2FPropertyIsEqualTo%3E"

baffin_bay <- request(url) |>
  req_perform() |>
  resp_body_json() |>
  toJSON(pretty = TRUE, auto_unbox = TRUE) |>
  st_read()

extract_seaice_concentration <- function(date, baffin_poly) {

  # print(date)

  date_format <- format(date, "%Y%m")
  # year_format <- format(date, "%Y")
  month_format <- format(date, "%b")
  day_format <- format(date, "%m")

  # ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02135/north/monthly/geotiff/01_Jan/N_197901_extent_v3.0.tif
  url <- glue::glue("/vsicurl/ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02135/north/monthly/geotiff/{day_format}_{month_format}/N_{date_format}_concentration_v3.0.tif")

  r <- rast(url)
  r

  r[r == 0 | r > 1000] <- NA
  r <- r / 10

  r |>
    extract(baffin_poly) |>
    rename(sie = 2) |>
    summarise(mean_sie = mean(sie, na.rm = TRUE)) |>
    pull(mean_sie)
}


v <- vect(baffin_bay)

baffin_poly <- project(v, "EPSG:3413")

extract_seaice_concentration <- possibly(extract_seaice_concentration, NA)

df <- tibble(
  dates = seq(as.Date("1979-01-01"), as.Date("2021-12-31"), by = "month")
) |>
  # slice(1:10) |>
  mutate(
    seaice_concentration = pbmclapply(dates, extract_seaice_concentration,
      baffin_poly = baffin_poly,
      mc.cores = 10
    )
  )

df_viz <- df |>
  unnest(seaice_concentration) |>
  mutate(date = lubridate::ymd(format(dates, "2000-%m-%d"))) |>
  mutate(year = lubridate::year(dates))

# Plot --------------------------------------------------------------------

df_viz |>
  mutate(seaice_concentration = seaice_concentration / 100) |>
  fill(seaice_concentration) |>
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = 0, ymax = seaice_concentration), fill = "#595b72") +
  scale_y_continuous(
    labels = scales::label_percent(),
    expand = c(0, 0),
    limits = c(0, 1)
  ) +
  labs(
    x = NULL,
    y = "Sea ice concentration",
    title = str_wrap("Monthly sea ice concentration in Baffin Bay between 1979 and 2021", 40),
    caption = "Data from ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02135/north/monthly/geotiff/ | @philmassicotte"
  ) +
  facet_wrap(~ lubridate::month(dates, label = TRUE, abbr = FALSE)) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 16, color = "white"),
    plot.title = element_text(
      color = "white",
      hjust = 0.5,
      family = "Baloo2",
      size = 40
    ),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

ggsave(
  here::here("graphs", "seaice_concentration_baffin_bay.png"),
  dpi = 300,
  width = 14,
  height = 8
)
