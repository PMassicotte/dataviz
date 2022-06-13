library(tidyverse)
library(ggpmthemes)
library(rcartocolor)
library(ggnewscale)

theme_set(theme_montserrat())

url <- "https://masie_web.apps.nsidc.org/pub//DATASETS/NOAA/G02135/north/daily/data/N_seaice_extent_daily_v3.0.csv"

df <- read_csv(
  url,
  col_select = c(
    year = 1,
    month = 2,
    day = 3,
    seaice_extent = 4
  ),
  skip = 1,
  col_types = cols(.default = col_number())
)

df

df <- df |>
  mutate(date = lubridate::make_date("2000", month, day), .before = 1) |>
  mutate(decade = paste0(year %/% 10 * 10, "'s"), .after = 1)

df

# Year 2022 ---------------------------------------------------------------

df_2022 <- df |>
  filter(year == 2022)

# Plot --------------------------------------------------------------------

p <- df |>
  ggplot(aes(
    x = date,
    y = seaice_extent,
    group = year,
    color = decade
  )) +
  geom_line(size = 0.2, alpha = 0.75) +
  scale_color_carto_d(
    palette = "PurpOr",
    guide = guide_legend(
      override.aes = list(size = 2, alpha = 1),
      title = NULL
    )
  ) +
  new_scale_color() +
  geom_line(data = df_2022, aes(color = "2022"), size = 0.5) +
  scale_color_manual(
    values = c("2022" = "#D0D1E6FF"),
    guide = guide_legend(
      override.aes = list(size = 2, alpha = 1),
      title = NULL,
      order = 1
    )
  ) +
  scale_x_date(date_labels = "%B") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    x = NULL,
    y = quote(Arctic ~ sea ~ ice ~ extent ~ (10^6 ~ x ~ km^{2})),
    color = NULL,
    caption = "Data from https://nsidc.org/ | @philmassicotte"
  ) +
  theme(
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#2B2D42", color = NA),
    panel.grid = element_line(color = "gray35", size = 0.1),
    plot.background = element_rect(fill = "#2B2D42", color = NA),
    plot.caption = element_text(colour = "grey75"),
    legend.background = element_rect(fill = "#2B2D42"),
    legend.key = element_rect(fill = "#2B2D42"),
    legend.text = element_text(color = "white"),
    axis.ticks = element_blank(),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white")
  )

ggsave(
  "graphs/arctic_seaice_extent.png",
  dpi = 300,
  width = 9,
  height = 5
)
