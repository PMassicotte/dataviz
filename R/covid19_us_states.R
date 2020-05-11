library(tidyverse)
library(sf)
library(ggpmthemes)
library(ggtext)

theme_set(theme_light_modified(base_family = "Roboto Condensed"))

file <-
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

df <- read_csv(file)

df

df %>%
  count(state, sort = TRUE)
df %>%
  filter(state == "New York") %>%
  filter(date == max(date)) %>%
  pull(cases) %>%
  sum()

df_by_state <- df %>%
  filter(date == max(date)) %>%
  group_by(state) %>%
  summarise(total_cases = sum(cases))

# https://www.thedataschool.co.uk/chris-meardon/hex-map-how-to-why-to-blogs-about-one-dashboard/
us_hex <- st_read("data/raw/Hex States Shapefile/HexStates.shp") %>%
  janitor::clean_names() %>%
  mutate_if(is.factor, as.character) %>%
  st_transform(crs = 3395)

us_hex_outline <- us_hex %>%
  st_union() %>%
  st_buffer(dist = 40000)

df <- inner_join(us_hex, df_by_state, by = c("state"))

# Plot --------------------------------------------------------------------

p <- df %>%
  ggplot() +
  geom_sf(aes(fill = total_cases), size = 0.5, color = "white") +
  geom_sf(
    data = us_hex_outline,
    color = "white",
    fill = "transparent",
    size = 1
  ) +
  geom_sf_text(
    aes(label = state_abbr, color = total_cases > 3000),
    size = 4,
    fontface = "plain",
    vjust = 2.5,
  ) +
  geom_sf_text(
    aes(
      label = scales::label_number_auto()(total_cases),
      color = total_cases > 3000
    ),
    size = 5,
    fontface = "bold"
  ) +
  coord_sf(crs = 3395) +
  rcartocolor::scale_fill_carto_c(
    palette = "Teal",
    trans = "log10",
    labels = scales::label_number_auto(),
    breaks = scales::breaks_log(n = 6)
  ) +
  scale_color_manual(guide = FALSE, values = c("black", "#E8E8E7")) +
  # rcartocolor::scale_color_carto_c(
  #     palette = "Teal",
  #     trans = "log10",
  #     breaks = scales::log_breaks(n = 8),
  #     direction = -1
  #   ) +
  guides(
    fill = guide_colorbar(
      barheight = unit(4, units = "mm"),
      barwidth = unit(200, units = "mm"),
      direction = "horizontal",
      ticks.colour = "#3c3c3c",
      title.position = "top",
      title.hjust = 0.5,
      label.theme = element_text(color = "white", size = 14),
      title.theme = element_text(color = "white", size = 18),
      title = "Number of confirmed cases"
    )
  ) +
  labs(
    title = "Number of confirmed cases of covid-19 in the USA",
    subtitle = glue::glue(
      "Number of confirmed cases: **{scales::label_number()(sum(df$total_cases))}** *(updated on {Sys.Date()})*"
    ),
    caption = "Data: https://github.com/nytimes/covid-19-data\nVisualization: @philmassicotte"
  ) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = "#3c3c3c"),
    plot.background = element_rect(fill = "#3c3c3c"),
    legend.background = element_rect(fill = "#3c3c3c"),
    strip.background = element_rect(fill = "#3c3c3c"),
    strip.text = element_text(
      color = "gray75",
      size = 24,
      face = "bold"
    ),
    plot.title = element_text(
      color = "white",
      hjust = 0.5,
      size = 28
    ),
    plot.caption = element_text(color = "gray75", size = 12),
    plot.subtitle = element_markdown(
      color = "gray75",
      hjust = 0.5,
      size = 18
    )
  )

pdf_file <- here::here("graphs", "covid19_states.pdf")
png_file <- here::here("graphs", "covid19_states.png")

ggsave(pdf_file,
  device = cairo_pdf,
  width = 10,
  height = 12
)

knitr::plot_crop(pdf_file)

bitmap <- pdftools::pdf_render_page(pdf_file, dpi = 600)
png::writePNG(bitmap, png_file)
