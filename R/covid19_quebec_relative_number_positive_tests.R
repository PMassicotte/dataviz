library(tidyverse)
library(ggpmthemes)

theme_set(theme_poppins())

df <-
  read_csv(
    "https://www.inspq.qc.ca/sites/default/files/covid/donnees/combine.csv?randNum=26659766"
  ) %>%
  janitor::clean_names() %>%
  select(date,
         total_number_of_cases = cas_total,
         number_of_tests = tests_volumetrie
  ) %>%
  drop_na() %>%
  mutate(date = as.Date(date, "%d/%m/%Y"))

df %>%
  mutate(percentage_positive_tests = total_number_of_cases / number_of_tests) %>%
  ggplot(aes(x = date, y = percentage_positive_tests)) +
  geom_line() +
  scale_x_date(date_breaks = "4 weeks", date_labels = "%B") +
  scale_y_continuous(
    labels = scales::label_percent(),
    breaks = scales::breaks_pretty(n = 8)
  ) +
  labs(
    x = NULL,
    y = "Percentage of positive tests",
    title = str_wrap(
      "Percentage of positive COVID-19 tests for the province of Quebec",
      50
    ),
    subtitle = str_wrap(
      "The absolute number of cases does not provide useful information. If you test more, you will find more cases. This graph shows the number of positive cases relative to the number of tests performed.",
      140
    ),
    caption = "Visualization: @philmassicotte\nData: https://www.inspq.qc.ca/covid-19/donnees"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(size = 8, lineheight = 1.15),
    plot.caption = element_text(size = 6, color = "gray50")
  )

ggsave(
  here::here("graphs/covid19_quebec_relative_number_positive_tests.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 5.16
)

pdftools::pdf_convert(
  here::here("graphs/covid19_quebec_relative_number_positive_tests.pdf"),
  format = "png",
  filenames = here::here("graphs/covid19_quebec_relative_number_positive_tests.png"),
  dpi = 300
)
