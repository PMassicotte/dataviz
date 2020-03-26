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
  filter(country_region == "Canada") %>%
  # filter(province_state == "Quebec") %>%
  group_by(province_state) %>%
  arrange(date) %>%
  mutate(daily_case = lead(case_confirmed) - case_confirmed) %>%
  filter(daily_case >= 1) %>%
  add_count() %>%
  filter(n >= 8) %>%
  ggplot(aes(x = date, y = daily_case, group = province_state)) +
  geom_line(size = 0.5) +
  scale_x_date(date_breaks = "3 weeks", date_labels = "%b-%d") +
  # geom_point() +
  facet_wrap(~province_state, ncol = 2) +
  labs(
    y = "Number of new daily cases",
    x = NULL,
    title = "Number of new daily cases of covid-19 in Canada",
    subtitle = "Showing provinces for which there are data for at least 8 days.",
    caption = "Data: https://github.com/CSSEGISandData/COVID-19\nVisualization: @philmassicotte"
  ) +
  theme(
    strip.text.x = element_text(hjust = 0, face = "bold", size = 12),
    strip.background = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, size = 14),
    plot.caption = element_text(size = 8)
  )

ggsave(
  here::here("graphs/covid19_daily_cases_canada.png"),
  dpi = 600,
  type = "cairo",
  width = 5.45,
  height = 4.68
)
