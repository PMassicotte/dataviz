library(tidyverse)
library(rvest)
library(ggpmthemes)
library(ggtext)

theme_set(theme_poppins())

url <-
  "https://en.wikipedia.org/wiki/List_of_Impractical_Jokers_episodes"

df <- read_html(url) %>%
  html_nodes("table") %>%
  html_table(fill = TRUE) %>%
  .[2:10] %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  as_tibble()

df_viz <- df %>%
  filter(str_starts(title, '\\"')) %>%
  separate_rows(losing_joker_s) %>%
  filter(losing_joker_s %in% c("Q", "Sal", "Murr", "Joe")) %>%
  count(losing_joker_s)

df_viz %>%
  mutate(losing_joker_s = fct_reorder(losing_joker_s, n)) %>%
  ggplot(aes(x = n, y = losing_joker_s)) +
  geom_col() +
  geom_text(
    aes(label = glue::glue("Lost challenges: {n}")),
    hjust = 1.1,
    color = "white",
    family = "Comfortaa",
    fontface = "bold"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(
    y = NULL,
    x = NULL,
    title = "*Impractical Jokers* biggest loosers",
    subtitle = "In 8 seasons, **Sal** and **Murr** have<br>lost more challenges than **Q** and **Joe**.",
    caption = "Data: https://en.wikipedia.org/wiki/List_of_Impractical_Jokers_episodes\nVisualization: @philmassicotte"
  ) +
  theme(
    plot.title = element_markdown(
      family = "Combo",
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = element_markdown(hjust = 0.5),
    plot.caption = element_text(
      color = "gray50",
      size = 8,
      hjust = 0
    ),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank()
  )

ggsave(
  here::here("graphs/", "impratical_jokers.png"),
  type = "cairo",
  width = 5.45,
  height = 4.68,
  dpi = 600
)
