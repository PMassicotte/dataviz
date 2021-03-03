library(tidyverse)
library(ggpmthemes)

theme_set(theme_light_modified(base_family = "Montserrat Alternates", base_size = 10))

theme_update(
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 10, color = "#4c4a4a")
)

df <- readxl::read_excel(
  "data/raw/seanoe_downloads.xls",
  skip = 3
) %>%
  janitor::clean_names() %>%
  mutate(date = lubridate::parse_date_time(date, orders = "YmdHMS")) %>%
  mutate(dataset = case_when(
    str_detect(dataset_title, "Green Edge") ~ "Green Edge",
    str_detect(dataset_title, "Malina") ~ "Malina"
  ))

df

df %>%
  count(dataset, landing_page_data_file)

# Time series -------------------------------------------------------------

df

df %>%
  count(date, sort = TRUE)

df_viz <- df %>%
  rename(date_time = date) %>%
  mutate(date = as.Date(date_time), .before = date_time)

df_viz %>%
  count(date, sort = TRUE)

df_viz <- df_viz %>%
  group_by(date, dataset, landing_page_data_file) %>%
  summarise(n = n()) %>%
  arrange(date) %>%
  group_by(dataset, landing_page_data_file) %>%
  mutate(total = cumsum(n))

# Plot the time series ----------------------------------------------------

p <- df_viz %>%
  ggplot(aes(x = date, y = total, color = dataset)) +
  geom_line() +
  facet_wrap(~landing_page_data_file, ncol = 1, labeller = labeller(
    landing_page_data_file = c(
      "Data File download" = "Number of downloads",
      "Landing Page" = "Number of views"
    )
  )) +
  labs(
    x = NULL,
    y = "Number of views/downloads",
    title = str_wrap("Cumulative numbers of downloads and views for the data hosted on SEANOE", 40)
  ) +
  theme(
    legend.title = element_blank(),
    strip.text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.title.position = "plot"
  )

ggsave(
  "graphs//timeseries_seanoe_view_and_download.pdf",
  device = cairo_pdf,
  width = 6,
  height = 5
)
