library(tidyverse)
library(sf)
library(ggpmthemes)
library(ggtext)
library(maps)
library(geofacet)
library(tigris)

rm(list = ls())

theme_set(theme_light_modified(base_family = "Roboto Condensed"))

file <-
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

df <- read_csv(file) %>%
  mutate(fips = parse_number(fips)) %>%
  filter(date == max(date, na.rm = TRUE))

df %>%
  count(date, sort = TRUE)

us_county <- usmap::us_map(regions = "counties") %>%
  as_tibble() %>%
  mutate(fips = parse_number(fips)) %>%
  select(-county)

us_county

df2 <- us_county %>%
  full_join(df, by = c("fips" = "fips", "full" = "state"))

df2

labels <- df %>%
  # distinct(abbr, fips, .keep_all = TRUE) %>%
  group_by(state) %>%
  summarise(
    total_cases = sum(cases, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE)
  ) %>%
  mutate(
    lab_total_cases = glue::glue(
      "<span style='color:#D1EEEA'>Cases: </span>**{scales::label_number(accuracy = 1)(total_cases)}**"
    )
  ) %>%
  mutate(
    lab_total_deaths = glue::glue(
      "<span style='color:#F8A07E'>Deaths: </span>**{scales::label_number(accuracy = 1)(total_deaths)}**"
    )
  ) %>%
  inner_join(distinct(us_county, abbr, full), by = c("state" = "full")) %>%
  drop_na(abbr)

sum(df$cases)
sum(labels$total_cases)

# setdiff(df$state, labels$full)

p <- df2 %>%
  drop_na(abbr) %>%
  ggplot(aes(x = x, y = y, group = group)) +
  geom_polygon(color = "white", aes(fill = cases), size = 0.1) +
  geom_richtext(
    data = labels,
    aes(
      x = -Inf,
      y = Inf,
      label = lab_total_cases
    ),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    family = "Open Sans",
    size = 3,
    color = "gray85",
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  geom_richtext(
    data = labels,
    aes(
      x = -Inf,
      y = Inf,
      label = lab_total_deaths
    ),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 2,
    family = "Open Sans",
    size = 3,
    color = "gray85",
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.5))) +
  scale_x_continuous(expand = expansion(mult = c(0.5, 0.5))) +
  facet_geo(~abbr, scales = "free", grid = "us_state_grid1") +
  rcartocolor::scale_fill_carto_c(
    palette = "DarkMint",
    trans = "log10",
    na.value = "transparent"
  ) +
  labs(
    title = "The covid-19 situation in the USA",
    caption = "Data: https://github.com/nytimes/covid-19-data\nVisualization: @philmassicotte",
    subtitle = glue::glue("Updated on {unique(df$date)}")
  ) +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(color = "white", hjust = 0.5, size = 30, margin = margin(t = 10)),
    plot.title.position = "plot",
    plot.caption = element_text(color = "gray75", size = 12),
    plot.subtitle = element_text(color = "gray75", size = 12, hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 18, face = "bold", color = "gray75"),
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c")
  )

# p <- df2 %>%
#   ggplot() +
#   geom_sf(aes(fill = cases), color = "#3c3c3c", size = 0.1) +
#   # facet_wrap(~state) +
#   # geom_polygon(color = "white", aes(fill = cases), size = 0.1) +
#   # facet_geo(~state, scales = "free") +
#   scale_fill_viridis_c(trans = "log", na.value = "white", option = "C") +
#   # coord_equal() +
#   coord_sf(crs = 2163) +
#   theme(
#     legend.position = "none",
#     axis.title = element_blank(),
#     axis.ticks = element_blank(),
#     panel.border = element_blank(),
#     panel.grid = element_blank(),
#     axis.text = element_blank()
#   )

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "covid19_counties.pdf")

ggsave(destfile,
  device = cairo_pdf,
  width = 12,
  height = 12
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "covid19_counties.png")
png::writePNG(bitmap, destfile)

# ggsave(
#   "graphs/covid19_counties.pdf",
#   device = cairo_pdf,
#   width = 14,
#   height = 10
# )
