library(tidyverse)
library(vroom)
library(ggpmthemes)
library(ggtext)

theme_set(theme_light_modified(base_family = "Maven Pro"))

rm(list = ls())

# https://github.com/datasciencecampus/mobility-report-data-extractor
# https://drive.google.com/drive/folders/1VFkLwK3vXm96ZXWv7HzSrL9aisE_0nJD

extract_data <- function(dir) {
  # dir <- "data/raw/google_mobility/CSV/USA/"

  category <- c(
    "Retail & recreation",
    "Grocery & pharmacy",
    "Parks",
    "Transit stations",
    "Workplaces",
    "Residential"
  )

  df <- fs::dir_ls(dir) %>%
    enframe(value = "data_file", name = NULL) %>%
    mutate(data = map(
      data_file,
      vroom,
      col_select = c(1:4),
      col_types = cols(
        value = col_double(),
        date = col_date(format = ""),
        origin = col_character()
      )
    )) %>%
    mutate(file_num = str_match(data_file, "-(\\d+)\\.csv")[, 2]) %>%
    mutate(file_num = parse_number(file_num)) %>%
    arrange(file_num) %>%
    mutate(category = rep(category, times = nrow(.) / 6))


  return(df)
}

dirs <- c(
  "data/raw/google_mobility/CSV/Canada/",
  "data/raw/google_mobility/CSV/USA/"
)

df <- map_df(dirs, extract_data) %>%
  unnest(data) %>%
  mutate(value = value / 100)

df_usa_states <- df %>%
  filter(origin == "USA") %>%
  filter(file_num > 6)

df_usa_global <- df %>%
  filter(origin == "USA") %>%
  filter(file_num <= 6)

df_canada_provinces <- df %>%
  filter(origin == "Canada") %>%
  filter(file_num > 6)

df_canada_global <- df %>%
  filter(origin == "Canada") %>%
  filter(file_num <= 6)

df_Québec <- df %>%
  filter(origin == "Canada") %>%
  filter(file_num %in% 67:72)

df_Québec %>%
  count(file_num)

# Plot --------------------------------------------------------------------

p <- ggplot() +
  geom_line(
    data = df_usa_states,
    aes(x = date, y = value, group = data_file),
    size = 0.25,
    color = "#5D8CA8",
    alpha = 0.1,
    show.legend = FALSE
  ) +
  geom_line(
    data = df_canada_provinces,
    aes(x = date, y = value, group = data_file),
    size = 0.25,
    color = "#D5695D",
    alpha = 0.2,
    show.legend = FALSE
  ) +
  geom_line(
    data = df_usa_global,
    aes(
      x = date,
      y = value,
      group = data_file,
      color = "USA"
    ),
    size = 0.5,
    alpha = 1
  ) +
  geom_line(
    data = df_canada_global,
    aes(
      x = date,
      y = value,
      group = data_file,
      color = "Canada"
    ),
    size = 0.5,
    alpha = 1
  ) +
  geom_line(
    data = df_Québec,
    aes(
      x = date,
      y = value,
      group = data_file,
      color = "Québec"
    ),
    size = 0.5,
    alpha = 1
  ) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    color = "gray75",
    size = 0.25
  ) +
  facet_wrap(~category, scales = "free_y") +
  scale_color_manual(
    breaks = c("USA", "Canada", "Québec"),
    values =
      c(
        "USA" = "#5D8CA8",
        "Canada" = "#D5695D",
        "Québec" = "#D3BA68"
      ),
    guide = guide_legend(
      label.position = "top",
      keywidth = unit(5, "cm"),
      label.theme = element_text(size = 12, color = "gray85", face = "bold")
    )
  ) +
  scale_y_continuous(
    labels = scales::label_percent(),
    breaks = scales::breaks_pretty()
  ) +
  scale_x_date(
    date_breaks = "10 days",
    date_labels = "%b %d",
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Community Mobility Reports in the US and Canada",
    subtitle = "Visits and length of stay at different places change compared to a baseline.<br>*The horizontal dashed baseline is the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020.*",
    caption = "Data: https://github.com/datasciencecampus/mobility-report-data-extractor\nVisualization: @philmassicotte"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#3c3c3c"),
    legend.key = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "gray75"),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(
      color = "gray50",
      hjust = 0,
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_markdown(color = "gray75", size = 10, lineheight = 1.25),
    plot.caption = element_text(color = "gray75", size = 6)
  )

ggsave(
  "graphs/covid19_google_mobility_report.png",
  type = "cairo",
  dpi = 600,
  width = 12,
  height = 6
)
