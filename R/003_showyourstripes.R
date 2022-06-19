library(tidyverse)
library(rcartocolor)
library(ggfx)

# https://showyourstripes.info/faq


url <-
  "https://berkeleyearth.org/wp-content/themes/client-theme/temperature-data/Canada-projection.txt"

df <- read_delim(
  url,
  skip = 51,
  na = "NaN",
  delim = "\t",
  col_select = c(
    year = 1,
    average_temperature = 2
  ),
  col_names = FALSE
)

df

# Plot --------------------------------------------------------------------

xx <- seq(1910, 2020, by = 20)

df |>
  filter(between(year, 1901, 2020)) |>
  ggplot(aes(x = year, y = 1, fill = average_temperature)) +
  geom_col(
    width = 1,
    color = "white",
    size = 0.1
  ) +
  geom_vline(
    xintercept = xx,
    size = 0.25,
    linetype = 2,
    alpha = 0.7,
    color = "#2B2D42"
  ) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = 0.06,
    ymax = 0.14,
    alpha = 0.3,
    fill = "#2B2D42"
  ) +
  with_shadow(
    annotate(
      "text",
      x = xx,
      y = 0.1,
      label = xx,
      size = 8,
      fontface = "bold",
      angle = 0,
      color = "white",
      alpha = 1
    ),
    colour = "#2B2D42",
    x_offset = 3,
    y_offset = 10.01,
    sigma = 8
  ) +
  paletteer::scale_fill_paletteer_c("pals::ocean.matter") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    caption = "Data from https://berkeleyearth.org | @philmassicotte"
  ) +
  theme(
    panel.border = element_blank(),
    panel.background = element_rect(fill = "#2B2D42", color = NA),
    panel.grid = element_line(color = "gray35", size = 0.1),
    plot.background = element_rect(fill = "#2B2D42", color = NA),
    plot.caption = element_text(colour = "grey60", size = 6),
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

ggsave(
  here::here("graphs", "003_showyourstripes.png"),
  dpi = 300,
  width = 9,
  height = 5
)
ggsave(
  here::here("graphs", "003_showyourstripes.png"),
  dpi = 300,
  width = 9,
  height = 5
)
