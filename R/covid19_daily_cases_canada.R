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

df_viz <- df %>%
  filter(country_region == "Canada") %>%
  filter(!str_detect(province_state, "Recovered|Diamond|Grand")) %>%
  group_by(province_state) %>%
  arrange(date) %>%
  mutate(daily_case = lead(case_confirmed) - case_confirmed) %>%
  mutate(total_case = max(case_confirmed))

df_viz %>%
  ggplot(aes(x = date, y = daily_case, group = province_state)) +
  geom_line(size = 0.5) +
  scale_x_date(date_breaks = "3 weeks", date_labels = "%b-%d") +
  # geom_point() +
  facet_wrap(~str_wrap(province_state, 20), ncol = 3) +
  labs(
    y = "Number of new daily cases",
    x = NULL,
    title = "Number of new daily cases of covid-19 in Canada",
    subtitle = glue::glue("Total number of confirmed cases: {sum(df_viz$daily_case, na.rm = TRUE)}"),
    caption = "Data: https://github.com/CSSEGISandData/COVID-19\nVisualization: @philmassicotte"
  ) +
  theme(
    strip.text.x = element_text(hjust = 0, face = "bold", size = 12),
    strip.background = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.caption = element_text(size = 8)
  )

ggsave(
  here::here("graphs/covid19_daily_cases_canada.png"),
  dpi = 600,
  type = "cairo",
  width = 7,
  height = 8
)
