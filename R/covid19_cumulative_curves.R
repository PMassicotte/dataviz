library(tidyverse)
library(ggpmthemes)
library(ggtext)

theme_set(theme_exo())

df <-
  read_csv(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  )

df <- df %>%
  pivot_longer(-c(1:4), names_to = "date", values_to = "case_confirmed") %>%
  janitor::clean_names() %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

df

df %>%
  count(country_region, sort = TRUE)

range(df$date)

total_case_confirmed <- df %>%
  group_by(country_region, date) %>%
  summarise(total_case_confirmed = sum(case_confirmed)) %>%
  ungroup()

total_case_confirmed <- total_case_confirmed %>%
  group_by(country_region) %>%
  filter(total_case_confirmed >= 100) %>%
  mutate(day = date - min(date)) %>%
  mutate(day = as.integer(day))

subset_total_case_confirmed <- total_case_confirmed %>%
  filter(country_region %in% c("Canada", "US", "Italy", "China", "Spain", "France"))

lab <- subset_total_case_confirmed %>%
  filter(day == max(day))


# Plot --------------------------------------------------------------------

subset_total_case_confirmed %>%
  ggplot(aes(x = day, y = total_case_confirmed, color = country_region)) +
  # geom_line(
  #   data = total_case_confirmed,
  #   aes(group = country_region),
  #   color = "gray65",
  #   size = 0.1
  # ) +
  geom_line(size = 0.75) +
  ggrepel::geom_text_repel(
    data = lab,
    aes(
      x = day,
      y = total_case_confirmed,
      label = glue::glue("{country_region} ({scales::number(total_case_confirmed)})")
    ),
    nudge_x = 1,
    # nudge_y = 1,
    hjust = -1,
    segment.colour = "gray75",
    segment.size = 0.25, max.iter = 1e5
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.5)),
    breaks = scales::breaks_pretty()
  ) +
  # scale_y_log10(labels = scales::label_number_auto()) +
  # annotation_logticks(
  #   sides = "l",
  #   color = "gray50",
  #   size = 0.25
  # ) +
  labs(
    x = "Days since cumulative confirmed case reached 100",
    y = "Cumulative number of confirmed cases",
    title = "Coronavirus trajectory in the world",
    subtitle = glue::glue(
      "The confirmed novel cases of Coronavirus (COVID-19) increase rapidly soon after it reaches the number of<br>100. <span style = 'color:#B48EADFF;'>Italy</span>, <span style = 'color:#EBCB8BFF;'>Spain</span>, <span style = 'color:#5991FF;'>France</span> and the <span style = 'color:#A3BE8CFF;'>USA</span> share a similar rate of novel cases. The situation is still early in <span style = 'color:#D08770FF;'>Canada</span>,<br>but it seems to follow the same pattern. On a positive note, the rate seems to stabilize in <span style = 'color:#BF616AFF;'>China</span>.<br>*Updated on {Sys.time()}*<br>"
    ),
    caption = "Data: https://github.com/CSSEGISandData/COVID-19\nVisualization: @philmassicotte"
  ) +
  scale_color_manual(
    values = c(
      "China" = "#BF616AFF",
      "Canada" = "#D08770FF",
      "Spain" = "#EBCB8BFF",
      "US" = "#A3BE8CFF",
      "Italy" = "#B48EADFF",
      "France" = "#5991FF"
    )
  ) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, color = "gray50"),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    # axis.ticks.y = element_line(color = "white", size = 0.15),
    plot.title = element_text(hjust = 0, family = "Lalezar"),
    plot.title.position = "plot",
    plot.caption = element_text(
      size = 6,
      color = "gray75",
      family = "Montserrat"
    ),
    plot.subtitle = element_markdown(
      size = 8,
      family = "Montserrat",
      lineheight = 1.5,
      margin = margin(b = unit(5, "lines"))
    )
  )

ggsave(
  here::here("graphs", "covid19_cumulative_curves.png"),
  type = "cairo",
  dpi = 600,
  width = 6.21,
  height = 5.17
)
