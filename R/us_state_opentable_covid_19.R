library(tidyverse)
library(sf)
library(ggpmthemes)
library(ggtext)

theme_set(theme_light_modified(base_family = "Roboto"))

us_state <-
  st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  st_transform(crs = 102009)

opentable <-
  read_csv("c:/Users/pmass/Downloads/state_of_industry_data.csv") %>%
  pivot_longer(-c(1:2), names_to = "date", values_to = "rate") %>%
  janitor::clean_names() %>%
  mutate(date = paste0(date, "/2020")) %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  mutate(name = tolower(name))

us <- opentable %>%
  filter(type == "state")

dates <- seq(min(us$date), max(us$date), length.out = 9)

us %>%
  filter(date == "2020-03-14")

p <- us %>%
  filter(as.character(date) %in% as.character(dates)) %>%
  inner_join(us_state %>% as_tibble(), by = c("name" = "ID")) %>%
  mutate(date = as.Date(date)) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = rate / 100), size = 0.1) +
  coord_sf(crs = 102009) +
  facet_wrap(~date) +
  scale_fill_gradient2(
    low = scales::muted("red"),
    mid = "white",
    high = scales::muted("blue"),
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = guide_legend(nrow = 1, title = NULL),
    aesthetics = "fill",
    labels = scales::label_percent()
  ) +
  labs(
    title = str_wrap("The state of the restaurant industry in the USA: a comparison  between 2019 and 2020", 50),
    subtitle = "Data interpretation: *For year-over-year comparisons by day, we compare to the same day of the week from the<br>same week in the previous year. For example, we’d compare Tuesday of week 11 in 2020 to Tuesday of week 11<br>in 2019. Only states or cities with 50+ restaurants in the sample are included.*",
    caption = "Visualization: @philmassicotte\nData Source: https://www.opentable.com/state-of-industry"
  ) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "#3c3c3c"),
    plot.background = element_rect(fill = "#3c3c3c"),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    text = element_text(color = "white"),
    strip.background = element_blank(),
    panel.border = element_blank(),
    axis.text = element_blank(),
    strip.text = element_text(
      hjust = 0,
      size = 14,
      family = "Roboto Condensed Bold",
      face = "bold",
      color = "white"
    ),
    legend.key = element_rect(colour = "white", size = 2),
    legend.box.background = element_blank(),
    legend.text = element_text(color = "white"),
    legend.background = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = unit(10, "lines"))
    ),
    plot.subtitle = element_markdown(
      family = "Roboto Condensed",
      color = "gray75",
      size = 12,
      lineheight = 1.25
    ),
    plot.caption = element_text(
      family = "Roboto Condensed Light",
      color = "gray75",
      size = 8,
      hjust = 0
    )
  )

# hrbrthemes::gg_check(p)

destfile <- here::here("graphs", "us_state_opentable_covid.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  width = 8,
  height = 8
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "us_state_opentable_covid.png")
png::writePNG(bitmap, destfile)
