library(tidyverse)
library(sf)
library(httr2)
library(jsonlite)
library(terra)
library(pbmcapply)

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

url <-
  "https://geo.vliz.be/geoserver/wfs?request=getfeature&service=wfs&version=1.1.0&typename=MarineRegions:iho&outputformat=json&filter=%3CPropertyIsEqualTo%3E%3CPropertyName%3Eid%3C%2FPropertyName%3E%3CLiteral%3E14A%3C%2FLiteral%3E%3C%2FPropertyIsEqualTo%3E"

baffin_bay <- request(url) |>
  req_perform() |>
  resp_body_json() |>
  toJSON(pretty = TRUE, auto_unbox = TRUE) |>
  st_read() |>
  vect()

extract_chla <- function(file) {
  mean_chla <- rast(file, 6) |>
    extract(baffin_bay) |>
    pull(chlor_a) |>
    mean(x = _, na.rm = TRUE)


  return(mean_chla)
}

files <-
  fs::dir_ls("/media/LaCie16TB/cci/", recurse = TRUE, glob = "*.nc")
files

df <- tibble(
  files,
  year = str_match(files, "(\\d{4})\\d{2}")[, 2],
  month = str_match(files, "\\d{4}(\\d{2})")[, 2]
) |>
  mutate(across(c(year, month), parse_number)) |>
  mutate(month = factor(month.name[month], month.name))

df_viz <- df |>
  mutate(mean_chla = pbmclapply(files, extract_chla, mc.cores = detectCores() - 1)) |>
  unnest(mean_chla)

df_viz

df_viz <- df_viz |>
  replace_na(list(mean_chla = 0))


# Plot --------------------------------------------------------------------

p <- df_viz |>
  filter(between(year, 1998, 2021)) |>
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = 0, ymax = mean_chla), fill = "#595b72") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = NULL,
    y = parse(text = "Chlorophyll-a~(mg~m^{-3})"),
    title = str_wrap(
      "Monthly chlorophyll-a concentration in seawater in Baffin Bay between 1998 and 2021",
      45
    ),
    caption = "Data from https://climate.esa.int/en/projects/ocean-colour/data | @philmassicotte"
  ) +
  facet_wrap(~month) +
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
  here::here("graphs", "005_chla_concentration_baffin_bay.png"),
  dpi = 300,
  width = 14,
  height = 9
)
