library(tidyverse)
library(ggpmthemes)

theme_set(theme_light_modified(base_family = "Oxanium"))

df <- tibble::tribble(
  ~object, ~length, ~ratio,
  "Hugin 1000", 4.5, 0.4444444444,
  "Bio Argo", 1.1, 1.818181818,
  "C-OPS", 0.33, 6.060606061,
  "Niskin bottle (1.7L)", 0.63, 3.174603175,
  "Disposable pipette", 0.15, 13.33333333,
  "Whatman GF/F filter", 0.047, 42.55319149,
  "Ultrapath 100 cm optical cable", 1, 2,
  "Spectrofluorimeter cuvette", 0.01, 200,
  "Québec Ocean Ford F-150", 6.3627, 0.314331966,
  "Diatoms", 1e-04, 20000,
  "Cup of coffee", 0.11, 18.18182
) %>%
  mutate(object = fct_reorder(object, ratio))

p <- df %>%
  ggplot(aes(
    x = ratio,
    y = object,
    color = factor(ratio)
  )) +
  # geom_col() +
  geom_point(size = 12) +
  geom_text(
    aes(label = round(ratio, digits = 2)),
    size = 3,
    color = "#3c3c3c"
  ) +
  scale_x_log10(
    breaks = scales::breaks_log(n = 8),
    labels = scales::label_number_si()
  ) +
  annotation_logticks(sides = "b", colour = "white") +
  labs(
    y = NULL,
    x = NULL,
    title = "The hitchhiking guide of the scientific distancing",
    subtitle = "How many of these items do you need to keep a distance of 2 meters?"
  ) +
  paletteer::scale_color_paletteer_d("vapoRwave::vapoRwave") +
  theme(
    panel.background = element_rect(fill = "#3c3c3c"),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, color = "gray75"),
    legend.position = "none",
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.title.position = "plot"
  )

ggsave("~/Desktop/distanciation_scientific.png",
  dpi = 600,
  type = "cairo",
  width = 8,
  height = 6
)
