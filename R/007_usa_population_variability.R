library(tidyverse)

# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

url <-
  "https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.xlsx?v=6874.3"

file <- curl::curl_download(url, tempfile())

df <- readxl::read_excel(file, skip = 4) |>
  janitor::clean_names() |>
  filter(state != "US")

names(df)

df <- df |>
  select(
    fips = federal_information_processing_standards_fips_code,
    contains("population")
  ) |>
  pivot_longer(
    contains("population"),
    names_to = c(NA, "year"),
    names_pattern = c("(population)_(\\d{4})"),
    names_transform = list(year = parse_number),
    values_to = "population"
  )

df

df <- df |>
  filter(year %in% c(1990, 2021)) |>
  drop_na(population) |>
  pivot_wider(
    names_from = year,
    names_prefix = "pop",
    values_from = population
  ) |>
  mutate(pop_diff = pop2021 - pop1990) |>
  mutate(pop_diff_percent = (pop2021 - pop1990) / ((pop2021 + pop1990) / 2)) |>
  mutate(increased = ifelse(pop_diff >= 0, TRUE, FALSE))

counties <- urbnmapr::get_urbn_map(map = "counties", sf = TRUE) |>
  rename(fips = county_fips)

counties_sf <- counties |>
  left_join(df)

range(counties_sf$pop_diff, na.rm = TRUE)
range(counties_sf$pop_diff_percent, na.rm = TRUE)

states <- urbnmapr::get_urbn_map(map = "states", sf = TRUE)

# Plot --------------------------------------------------------------------

limit <-
  ceiling(max(abs(counties_sf$pop_diff_percent), na.rm = TRUE) * 10) / 10 * c(-1, 1)

p <- ggplot() +
  geom_sf(
    data = counties_sf,
    aes(fill = pop_diff_percent),
    size = 0.1,
    color = "grey45"
  ) +
  geom_sf(
    data = states,
    size = 0.1,
    color = "grey25",
    fill = NA
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Orange-Blue-White Diverging",
    limit = limit,
    labels = scales::label_percent(),
    breaks = scales::breaks_pretty(),
    na.value = NA,
    guide = guide_colorbar(
      title = NULL,
      label.position = "top",
      label.theme = element_text(
        family = "Montserrat",
        size = 8,
        color = "white"
      ),
      barwidth = unit(10, "cm"),
      barheight = unit(0.2, "cm"),
      order = 2,
      ticks.colour = "#3c3c3c"
    )
  ) +
  labs(title = str_wrap("Relative demographic changes in USA between 1990 and 2021", 40)) +
  theme_void(base_family = "Ubuntu") +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.margin = margin(b = 15, t = 15),
    strip.text = element_text(size = 24, face = "bold"),
    panel.background = element_rect(fill = "#3c3c3c", color = NA),
    plot.background = element_rect(fill = "#3c3c3c", color = NA),
    plot.title = element_text(
      family = "Montserrat",
      color = "white",
      size = 24,
      hjust = 0.5
    ),
    plot.title.position = "plot"
  )

p

ggsave(
  here::here("graphs", "007_usa_population_variability.png"),
  dpi = 300,
  width = 7,
  height = 5
)
