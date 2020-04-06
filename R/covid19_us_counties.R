library(tidyverse)
library(sf)
library(ggpmthemes)
library(ggtext)
library(maps)
library(geofacet)
library(tigris)

rm(list = ls())

theme_set(theme_light_modified(base_family = "Roboto Condensed"))

file <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

df <- read_csv(file) %>%
  mutate(fips = parse_number(fips)) %>%
  filter(date == max(date, na.rm = TRUE))


df %>%
  count(date, sort = TRUE)

fips <- read_csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/county_fips_master.csv") %>%
  mutate(ID = paste(str_to_lower(state_name), str_to_lower(long_name), sep = ",")) %>%
  select(fips, county_name, state_abbr, ID)

us_county <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))



p <- us_county %>%
  left_join(select(df, state, state_abb, fips, cases)) %>%
  filter(state_abb %in% state.abb) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "white", aes(fill = cases), size = 0.1) +
  facet_geo(~state_abb, scales = "free") +
  scale_fill_viridis_c(trans = "log10") +
  # coord_equal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

ggsave(
  "graphs/covid19_counties.pdf",
  device = cairo_pdf,
  width = 10,
  height = 10
)
