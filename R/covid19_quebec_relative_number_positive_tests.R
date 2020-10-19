library(tidyverse)
library(ggpmthemes)
library(ggforce)

theme_set(theme_poppins())

# To have french dates
# https://askubuntu.com/questions/76013/how-do-i-add-locale-to-ubuntu-server
# sudo nano /etc/locale.gen
# sudo locale-gen fr_FR.UTF-8

Sys.setlocale("LC_TIME", "fr_CA.UTF-8")

# df <-
#   read_csv(
#     "https://www.inspq.qc.ca/sites/default/files/covid/donnees/combine.csv?randNum=26659766"
#   ) %>%
#   janitor::clean_names() %>%
#   select(date,
#     total_number_of_cases = cas_total,
#     number_of_tests = tests_volumetrie
#   ) %>%
#   drop_na() %>%
#   mutate(date = as.Date(date, "%d/%m/%Y")) %>%
#   mutate(percentage_positive_tests = total_number_of_cases / number_of_tests)

df_case <- read_csv("~/Downloads/chart.csv") %>%
  janitor::clean_names() %>%
  mutate(total_number_of_cases = par_lien_epidemiologique + en_laboratoire) %>%
  mutate(date = as.Date(date_de_declaration_du_cas))

df_test <- read_csv("~/Downloads/chart (1).csv") %>%
  janitor::clean_names() %>%
  rename(number_of_tests = nombre_de_tests_de_depistage) %>%
  mutate(date = as.Date(date_danalyse_du_prelevement))

df <- df_case %>%
  inner_join(df_test) %>%
  mutate(percentage_positive_tests = total_number_of_cases / number_of_tests) %>%
  drop_na(date, percentage_positive_tests)

date_breaks <- seq(min(df$date), max(df$date), length.out = 4)

p <- df %>%
  ggplot(aes(x = date, y = percentage_positive_tests)) +
  geom_line() +
  scale_x_date(date_breaks = "4 weeks", date_labels = "%d %B", expand = expansion(mult = c(0.1, 0.2))) +
  scale_y_continuous(
    labels = scales::label_percent(),
    breaks = scales::breaks_pretty(n = 8)
  ) +
  labs(
    x = NULL,
    y = "Pourcentage de tests positifs",
    title = str_wrap(
      glue::glue("Pourcentage de tests COVID-19 positifs pour la province de Québec ({paste(format(range(df$date), '%d %b %Y'), collapse = ' - ')})"),
      50
    ),
    subtitle = str_wrap(
      "Le nombre absolu de cas n'est pas une information utile. Si vous testez plus, vous trouverez plus de cas. Ce graphique montre le nombre de cas positifs par rapport au nombre de tests effectués.",
      140
    ),
    caption = "Visualisation: @philmassicotte\nDonnées: https://www.inspq.qc.ca/covid-19/donnees"
  ) +
  geom_mark_circle(aes(
    filter = date %in% date_breaks | percentage_positive_tests == max(percentage_positive_tests) | total_number_of_cases == max(total_number_of_cases),
    group = date,
    label = date,
    description = glue::glue("{total_number_of_cases} cas positifs\n{number_of_tests} tests effectués")
  ),
  expand = unit(0.75, "mm"),
  size = 0.25,
  con.size = 0.25,
  label.fontsize = c(8, 6),
  con.colour = "#db3a34",
  color = "#db3a34",
  label.colour = "#084c61",
  label.buffer = unit(0.5, "cm"),
  label.family = "Roboto Condensed"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(size = 8, lineheight = 1.15),
    plot.caption = element_text(size = 6, color = "gray50")
  )

ggsave(
  here::here("graphs/covid19_quebec_relative_number_positive_tests.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 5.16
)

pdftools::pdf_convert(
  here::here("graphs/covid19_quebec_relative_number_positive_tests.pdf"),
  format = "png",
  filenames = here::here("graphs/covid19_quebec_relative_number_positive_tests.png"),
  dpi = 600
)
