library(tidyverse)
library(tibbletime)
library(ggpmthemes)

theme_set(theme_light_modified(base_family = "Roboto Condensed"))

# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

file <- curl::curl_download(
  "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-05-11.xlsx",
  destfile = tempfile(fileext = "xlsx")
)

df <- readxl::read_excel(file) %>%
  janitor::clean_names() %>%
  arrange(countries_and_territories, date_rep) %>%
  mutate(date_rep = as.Date(date_rep))

df <- df %>%
  group_nest(countries_and_territories) %>%
  mutate(total_deaths = map_dbl(data, ~ sum(.$deaths, na.rm = TRUE))) %>%
  filter(total_deaths >= 100) %>%
  unnest(data)

rolling_mean <- rollify(.f = ~ mean(.x), window = 7)

df_viz <- df %>%
  # filter(countries_and_territories %in% c("Algeria")) %>%
  arrange(countries_and_territories, date_rep) %>%
  group_by(countries_and_territories) %>%
  mutate(rowid = 1:n()) %>%
  mutate(rolling_deaths = rolling_mean(deaths)) %>%
  drop_na(rolling_deaths) %>%
  mutate(id = detect_index(rolling_deaths, ~ . >= 1)) %>%
  filter(rowid >= id) %>%
  mutate(date = date_rep - min(date_rep)) %>%
  mutate(date = as.numeric(date)) %>%
  mutate(relative_deaths = rolling_deaths / max(rolling_deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    countries_and_territories =
      str_replace_all(countries_and_territories, "_", " ")
  ) %>%
  mutate(countries_and_territories = str_wrap(
    countries_and_territories,
    15
  ))

max_death <- df %>%
  group_by(countryterritory_code) %>%
  filter(deaths == max(deaths, na.rm = TRUE)) %>%
  ungroup() %>%
  select(countryterritory_code, deaths)

lab <- df_viz %>%
  group_by(countries_and_territories) %>%
  filter(relative_deaths == max(relative_deaths)) %>%
  distinct(countries_and_territories, .keep_all = TRUE) %>%
  select(countries_and_territories, countryterritory_code, date) %>%
  left_join(max_death, by = "countryterritory_code")

# Plot --------------------------------------------------------------------

p <- df_viz %>%
  ggplot(aes(x = date, y = relative_deaths)) +
  geom_line(
    data = select(df_viz, -countries_and_territories),
    aes(x = date, y = relative_deaths, group = countryterritory_code),
    color = "gray80",
    size = 0.1,
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_line(size = 0.25) +
  facet_wrap(~countries_and_territories) +
  scale_y_continuous(
    labels = scales::label_percent(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = "Number of days since the first confirmed death",
    y = "Relative death toll",
    title = "COVID-19 deaths relative to each country's highest daily death toll",
    caption = "Data: https://www.ecdc.europa.eu\nVisualization: @philmassicotte"
  ) +
  geom_point(
    data = lab,
    aes(x = date, y = 1),
    color = "#f97443",
    size = 1
  ) +
  geom_text(
    data = lab,
    aes(
      x = date,
      y = 1,
      label = glue::glue("{deaths} deaths")
    ),
    vjust = 0.15,
    hjust = -0.1,
    size = 3
  ) +
  theme(
    aspect.ratio = 0.75,
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", size = 0.2),
    axis.ticks = element_blank(),
    axis.text = element_text(family = "Kameron"),
    axis.title = element_text(color = "gray50"),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(size = 8)
  )

ggsave(
  "graphs/covid19_relative_death_per_country.png",
  type = "cairo",
  dpi = 600,
  width = 10,
  height = 10
)
