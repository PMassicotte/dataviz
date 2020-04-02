library(tidyverse)
library(rvest)
library(glue)
library(magick)
library(grid)
library(ggpmthemes)

theme_set(theme_exo())

extract_assistance <- function(url) {
  year <- str_match(url, "\\d{4}") %>%
    parse_number()

  df <- read_html(url) %>%
    html_table(header = TRUE) %>%
    .[[1]] %>%
    .[-1, 1:4] %>%
    as_tibble() %>%
    set_names(c("rank", "team", "game_played_at_home", "total_assistance")) %>%
    type_convert() %>%
    mutate(season = glue("{year-1}-{year}"))

  return(df)
}

year <- 2001:2019
urls <-
  glue("http://www.espn.com/nhl/attendance/_/year/{year}", year = year)

df <- map(urls, extract_assistance) %>%
  .[map(., function(x) {
    dim(x)[1]
  }) > 0] %>%
  bind_rows()

df

# Plot --------------------------------------------------------------------

img <-
  image_read("http://content.sportslogos.net/logos/1/1736/full/1651_anaheim_ducks-primary-2014.gif") %>%
  image_convert(type = "grayscale") %>%
  image_transparent("white") %>%
  image_flatten()
  # image_colorize(opacity = 50, "gray75")

g <- rasterGrob(img, interpolate = TRUE)

df %>%
  group_nest(team) %>%
  slice(1:4) %>%
  unnest(data) %>%
  mutate(year_start = parse_number(str_extract(season, "^\\d{4}"))) %>%
  mutate(season = fct_inorder(season)) %>%
  ggplot(aes(
    year_start,
    y = total_assistance / game_played_at_home,
    group = team
  )) +
  annotation_custom(
    g,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_line(color = "red", size = 2) +
  scale_x_continuous(
    labels = function(x) {
      glue("{x}-{x+1}")
    }
  ) +

  facet_wrap(~team) +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "gray75"),
    panel.background = element_rect(fill = alpha("gray75", 0.5)),
    panel.grid = element_blank()
  )

