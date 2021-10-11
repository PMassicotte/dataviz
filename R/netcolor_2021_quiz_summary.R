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

files <- fs::dir_ls("data/raw/", regexp = "quizz")

read_quizz_results <- function(file, sheet = "RawReportData Data") {

  readxl::read_excel(file,
    sheet = sheet
  ) %>%
    janitor::clean_names() %>%
    mutate(quiz = file) %>%
    select(quiz, player, question, score_points)
}

df <- map_df(files, read_quizz_results)

df

df_viz <- df %>%
  group_by(quiz, player) %>%
  summarise(total_score_points = sum(score_points)) %>%
  slice_max(total_score_points, n = 10) %>%
  ungroup()

df_viz

df_viz <- df_viz %>%
  mutate(quiz = case_when(
    str_detect(quiz, "quizz 1") ~ "Quiz #1",
    str_detect(quiz, "quizz 2") ~ "Quiz #2",
    str_detect(quiz, "quizz 3") ~ "Quiz #3"
  ))

p <- df_viz %>%
  mutate(player = reorder_within(player, total_score_points, quiz)) %>%
  ggplot(aes(x = total_score_points, y = player)) +
  geom_col(fill = "gray50") +
  geom_text(
    aes(label = glue::glue("{total_score_points} pts.")),
    size = 3,
    hjust = 1.25,
    color = "white",
    family = "Montserrat"
  ) +
  scale_y_reordered() +
  labs(
    title = str_wrap("Top players of the NetCOLOR quizzes", 20),
    x = "Total points",
    y = NULL
  ) +
  facet_wrap(~quiz, scales = "free", ncol = 1) +
  theme(
    axis.text = element_text(size = 6, color = "white"),
    plot.title = element_text(
      color = "white",
      family = "Faster One",
      face = "plain",
      size = 28,
    ),
    plot.subtitle = element_text(color = "white"),
    strip.text = element_text(color = "white", size = 18),
    axis.title = element_text(color = "white"),
    panel.grid = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "#4c4c4c"),
    plot.background = element_rect(fill = "#4c4c4c")
  )

ggsave(
  here::here("graphs/netcolor_quiz_summary.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 8
)

ggsave(
  here::here("graphs/netcolor_quiz_summary.png"),
  dpi = 300,
  width = 6,
  height = 8
)
