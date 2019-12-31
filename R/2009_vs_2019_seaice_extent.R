library(tidyverse)
library(curl)
library(ggpmthemes)
library(glue)
library(ggtext)

theme_set(theme_poppins())

file <-
  "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/daily/data/N_seaice_extent_daily_v3.0.csv"

raw_data <- curl_fetch_memory(file)
sea_ice_extent <- rawToChar(raw_data$content)

sea_ice_extent <- sea_ice_extent %>%
  read_csv(
    skip = 2,
    col_names = c("year", "month", "day", "extent", "missing", "source")
  ) %>%
  dplyr::select(year:extent) %>%
  mutate(day = parse_number(day)) %>%
  mutate(month = parse_number(month)) %>%
  mutate(month2 = month.name[month]) %>%
  mutate(month2 = factor(month2, month.name))

df_viz <- sea_ice_extent %>%
  filter(year %in% c(2009, 2019)) %>%
  mutate(date = as.Date(paste(year, month, day), format = "%Y %m %d")) %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(date2 = as.Date(paste("2019", yday), format = "%Y %j"))

df_ribbon <- df_viz %>%
  select(-c(month:day), -month2, -date) %>%
  pivot_wider(names_from = year, values_from = extent) %>%
  mutate(difference = (`2009` - `2019`) / `2009`)

subtitle <-
  glue("On average between <b style='color:#facd0e'>2009</b> and <b style='color:#f4730e'>2019</b>, the sea ice extent in the Arctic decreased by {round(mean(df_ribbon$difference * 100, na.rm = TRUE))}%.")

df_viz %>%
  ggplot(aes(x = date2, y = extent, color = factor(year))) +
  geom_ribbon(
    data = df_ribbon,
    aes(x = date2, ymin = `2009`, ymax = `2019`),
    inherit.aes = FALSE,
    fill = "gray85",
    color = NA,
    alpha = 0.5
  ) +
  geom_line() +
  xlab(NULL) +
  ylab(bquote("Ice extent" ~ (km ^ 2 ~ 10 ^ 6))) +
  scale_x_date(date_labels = "%B") +
  labs(
    title = "Arctic sea ice extent: a decade of changes",
       subtitle = subtitle,
    caption = "Data: National Snow & Ice Data Center | Visualizarion: @philmassicotte"
  ) +
  scale_color_manual(
    values = c(`2009` = "#facd0e",
               `2019` = "#f4730e"),
    guide = guide_legend(
      override.aes = list(size = 4),
      label.position = "top",
      direction = "horizontal"
    )
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid = element_line(color = "#B6D2D6", size = 0.1),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      family = "Paytone One",
      color = "#ADD0D7"
    ),
    legend.position = "none",
    plot.subtitle = element_markdown(
      hjust = 0.5,
      family = "Heebo",
      color = "#A7CAD0"
    ),
    panel.background = element_rect(fill = "#025B6D"),
    plot.background = element_rect(fill = "#025B6D"),
    axis.text = element_text(color = "#A7CAD0"),
    axis.title = element_text(color = "#A7CAD0"),
    plot.caption = element_text(color = "#A7CAD0", size = 8)
  )

ggsave(
  here::here("graphs", "2009_vs_2019_sea_ice_extent.png"),
  type = "cairo",
  height = 5,
  width = 5 * 1.6
)

