library(tidyverse)
library(cranlogs)
library(ggtext)
library(ggpmthemes)

theme_set(theme_light_modified(base_family = "Exo"))

last_year <- cran_downloads(
  packages = "ggplot2",
  from = "2019-01-01",
  to = "2019-03-20"
)

this_year <- cran_downloads(
  packages = "ggplot2",
  from = "2020-01-01",
  to = "2020-03-20"
)

df <- bind_rows(last_year, this_year) %>%
  as_tibble() %>%
  mutate(yday = lubridate::yday(date)) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(date2 = as.Date(paste0("2020-", yday), "%Y-%j"))

df %>%
  ggplot(aes(x = date2, y = count, color = factor(year))) +
  geom_line() +
  scale_x_date(date_labels = "%b-%d", date_breaks = "2 weeks") +
  scale_color_manual(
    values = c(
      "2019" = "#4B878BFF",
      "2020" = "#D01C1FFF"
    )
  ) +
  labs(
    x = NULL,
    y = "Number of download",
    title = "Do people do more data visualization<br>during the **covid-<span style = 'color:#ffab02;'>19</span>** quarantine?",
   subtitle = "Number of daily downloads of **ggplot2** for **<span style = 'color:#4B878BFF;'>2019</span>** and **<span style = 'color:#D01C1FFF;'>2020</span>**. There is a increase number of downloads<br>occuring at the end of February 2020. Is it because people have more time to do *data visualization*?",
   caption = "Visualization: @philmassicotte<br>Data: downloaded from the *cranlogs* R package"
  ) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.2),
    panel.spacing.y = unit(2, "lines"),
    axis.text = element_text(color = "gray50"),
    axis.title = element_text(color = "gray40"),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(family = "Roboto Condensed Light", lineheight = 1.25),
    plot.title = element_markdown(
      hjust = 0.5,
      margin = margin(b = unit(10, "lines")),
      family = "Alegreya Sans SC"
    ),
    # axis.title.y = element_text(hjust = 1),
    plot.caption = element_markdown(
      family = "Roboto Condensed Light",
      color = "gray50",
      size = 8
    )
  )

ggsave(
  here::here("graphs", "ggplot2_cran_download.png"),
  type = "cairo",
  dpi = 600,
  width = 7,
  height = 5
)
