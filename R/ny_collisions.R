library(tidyverse)
library(ggpmthemes)

# https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95

theme_set(theme_light_modified(base_family = "Oxanium"))

df <- vroom::vroom("~/Downloads/Motor_Vehicle_Collisions_-_Crashes.csv") %>%
  janitor::clean_names()

df <- df %>%
  mutate(crash_date = as.Date(crash_date, "%m/%d/%Y")) %>%
  mutate(year = lubridate::year(crash_date)) %>%
  mutate(month = lubridate::month(crash_date, label = TRUE))

range(df$crash_date)

df %>%
  count(borough)

df %>%
  count(borough, number_of_pedestrians_killed) %>%
  arrange(desc(number_of_pedestrians_killed))

df %>%
  count(year, month, sort = TRUE) %>%
  ggplot(aes(x = month, y = n, group = year, color = factor(year))) +
  geom_line() +
  labs(
    x = NULL,
    y = "Number of collisions"
  )
