# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Some visualizations based on the quiz #1 made during the first
# day of NetCOLOR 2021.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

library(tidyverse)
library(tidytext)
library(ggpmthemes)

# Set default ggplot2 font size and font family
# devtools::install_github("PMassicotte/ggpmthemes")
theme_set(theme_light_modified(base_family = "Montserrat Alternates", base_size = 10))

theme_update(
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 10, color = "#4c4a4a")
)

df <- readxl::read_excel("data/raw/NetCOLOR quizz 1.xlsx",
  sheet = "RawReportData Data"
) %>%
  janitor::clean_names()

df

df_viz <- df %>%
  select(
    question_number,
    question,
    player,
    answer,
    correct_incorrect,
    answer_time_seconds,
    score_points
  )

df_viz

# First plot --------------------------------------------------------------

p <- df_viz %>%
  filter(correct_incorrect == "Correct") %>%
  mutate(player = reorder_within(player, -answer_time_seconds, question)) %>%
  mutate(question = str_wrap(question, 40)) %>%
  mutate(question = fct_inorder(question)) %>%
  ggplot(aes(x = answer_time_seconds, y = player, fill = score_points)) +
  geom_col() +
  geom_text(
    aes(label = glue::glue("{score_points} pts.")),
    size = 1.75,
    hjust = -0.25,
    color = "white",
    family = "Montserrat"
  ) +
  scale_y_reordered() +
  scale_x_continuous(
    limits = c(0, 20),
    expand = expansion(mult = c(0, 0.05))
  ) +
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Blue") +
  facet_wrap(~question, scales = "free", ncol = 2) +
  labs(
    title = "The fast and furious of NetCOLOR",
    subtitle = "Only showing the participants who had the right answers.",
    x = "Time to answer (seconds)",
    y = NULL
  ) +
  theme(
    axis.text = element_text(size = 6, color = "white"),
    plot.title = element_text(
      color = "white",
      family = "Faster One",
      face = "plain",
      size = 28,
    ),
    plot.subtitle = element_text(color = "white"),
    strip.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    panel.grid = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "#5c5c5c"),
    plot.background = element_rect(fill = "#4c4c4c")
  )

ggsave(
  "graphs/netcolor_quiz_01.pdf",
  device = cairo_pdf,
  width = 9,
  height = 15
)

# Simon vs SimleyFace -----------------------------------------------------

df_viz2 <- df_viz %>%
  # filter(correct_incorrect == "Correct") %>%
  filter(player %in% c("Simon B", "SmileyFace")) %>%
  mutate(question_number = parse_number(question_number)) %>%
  group_by(player) %>%
  arrange(question_number) %>%
  mutate(across(c(answer_time_seconds, score_points), ~ cumsum(.), .names = "cum_{.col}")) %>%
  ungroup()

df_viz2

df_viz2 %>%
  group_by(player) %>%
  filter(cum_answer_time_seconds == max(cum_answer_time_seconds))

arrow <- tibble(
  x = c(8, 8),
  xend = c(9.8, 9.8),
  y = c(73, 86),
  yend = c(66, 79),
  player = c("Simon B", "SmileyFace"),
  label = c(62.5, 73.4)
)

p <- df_viz2 %>%
  ggplot(aes(x = question_number, y = cum_answer_time_seconds)) +
  geom_line(color = "gray75", size = 1.5) +
  geom_point(aes(size = answer_time_seconds, color = correct_incorrect)) +
  geom_text(
    data = arrow,
    aes(
      x = x,
      y = y,
      label = glue::glue("Finished in {label} sec.")
    ),
    size = 3,
    hjust = 1.1,
    color = "white"
  ) +
  geom_text(
    aes(label = cum_score_points),
    size = 2,
    color = "white",
    fontface = "bold",
    family = "Roboto"
  ) +
  geom_curve(
    data = arrow,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    curvature = -0.2,
    size = 0.5,
    arrow = arrow(length = unit(0.05, "inch")),
    color = "gray85"
  ) +
  scale_color_manual(
    breaks = c("Correct", "Incorrect"),
    values = c("#5CB85C", "#D43F3A"),
    guide = guide_legend(
      override.aes = list(size = 5),
      label.theme = element_text(color = "white", family = "Exo")
    )
  ) +
  scale_x_continuous(breaks = 1:10, expand = expansion(mult = c(0.1, 0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  facet_wrap(~player) +
  scale_size(
    range = c(6, 12),
    guide = "none"
  ) +
  labs(
    title = "The battle of the Titans",
    x = "Question #",
    y = "Cumulative time to answer (seconds)",
    subtitle = str_wrap(
      "Both players finished with the same score (6403 points). However, Simon B won the battle because his cumulative answering time was lower than SmileyFace. The numbers in the circles represent the cumulative scores.",
      120
    )
  ) +
  theme(
    axis.text = element_text(size = 6, color = "white"),
    plot.title = element_text(
      color = "white",
      family = "Faster One",
      face = "plain",
      size = 28,
      hjust = 0.5
    ),
    plot.title.position = "plot",
    plot.subtitle = element_text(
      color = "gray75",
      size = 8,
      hjust = 0.5,
      lineheight = 1.25
    ),
    strip.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    panel.grid = element_line(size = 0.1, color = "gray50"),
    panel.background = element_rect(fill = "#5c5c5c"),
    plot.background = element_rect(fill = "#4c4c4c"),
    legend.background = element_rect(fill = "#4c4c4c"),
    legend.key = element_rect(fill = "#4c4c4c"),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave(
  "graphs/netcolor_quiz_01_winners.pdf",
  device = cairo_pdf,
  width = 7,
  height = 5
)
