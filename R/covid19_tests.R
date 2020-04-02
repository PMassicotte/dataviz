library(tidyverse)
library(ggpmthemes)
library(ggtext)

theme_set(theme_light_modified(base_family = "Roboto Condensed"))

df <- read_csv("c:/Users/pmass/Downloads/covid-19-tests-country.csv") %>%
  janitor::clean_names()

df %>%
  filter(str_detect(entity, "Canada")) %>%
  filter(!str_detect(entity, "National")) %>%
  mutate(entity = fct_reorder(entity, total_covid_19_tests)) %>%
  ggplot(aes(x = total_covid_19_tests, y = entity)) +
  geom_col()
