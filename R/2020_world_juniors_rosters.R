library(tidyverse)
library(rvest)
library(ggflags) # devtools::install_github("rensa/ggflags")
library(ggpmthemes)
library(ggforce)

theme_set(theme_poppins(base_family = "Baloo"))

rm(list = ls())


# All countries except Canada ---------------------------------------------

url <- c(
  "https://www.tsn.ca/2020-world-juniors-rosters-group-a-1.1415290",
  "https://www.tsn.ca/2020-world-juniors-rosters-group-b-1.1415294"
)

player_info <- function(roster) {
  names(roster) <- roster[1, ]
  roster <- roster[-1, ]

  roster <- roster %>%
    filter(Name != "")

  i <- which(grepl("^[[:upper:]]+$", roster$Name))

  positions <- c(
    rep(roster$Name[i[1]], i[2] - i[1]),
    rep(roster$Name[i[2]], i[3] - i[2]),
    rep(roster$Name[i[3]], (nrow(roster) - i[3]) + 1)
  ) %>%
    str_trim() %>%
    str_squish()

  res <- roster %>%
    mutate(position = positions) %>%
    filter(Team != "" & Age != "Age") %>%
    janitor::clean_names() %>%
    readr::type_convert() %>%
    as_tibble()

  return(res)
}

extract_roster <- function(url) {
  # url <- url[3]

  teams <- read_html(url) %>%
    html_nodes("b") %>%
    html_text()

  roster <- read_html(url) %>%
    html_table()

  res <- map(roster, player_info) %>%
    set_names(teams) %>%
    bind_rows(.id = "team")

  return(res)
}

df1 <- map_df(url, extract_roster) %>%
  select(name, team, age, height = ht, weight_lbs = wt, position) %>%
  mutate(position = str_to_title(position)) %>%
  mutate(position = case_when(
    position == "Goaltenders" ~ "Goaltender",
    position == "Forwards" ~ "Forward",
    TRUE ~ position
  ))

# Canada ------------------------------------------------------------------

url <- "https://www.hockeycanada.ca/en-ca/team-canada/men/junior/2019-20/world-championship/stats/team-rosters?teamid=175"

df2 <- url %>%
  read_html() %>%
  html_table() %>%
  .[c(1, 3)] %>%
  bind_rows() %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(team = "CANADA") %>%
  select(
    name = player,
    team,
    position = pos,
    height,
    weight_lbs = weight
  )


# Merge data and some cleaning --------------------------------------------

df <- bind_rows(df1, df2) %>%
  mutate(country = countrycode::countrycode(team, "country.name", "iso2c")) %>%
  mutate(country = tolower(country)) %>%
  mutate(weight_kg = weight_lbs * 0.453592) %>%
  separate(height, into = c("h1", "h2"), sep = "'", convert = TRUE) %>%
  mutate(height_feet = h1 + (h2 / 12)) %>%
  select(-h1, -h2) %>%
  mutate(height_meter = height_feet * 0.3048) %>%
  mutate(height_to_weight = height_meter / weight_kg)

df %>%
  count(position)

f <- df %>%
  distinct(country, .keep_all = TRUE)

df %>%
  mutate(country = fct_reorder(country, weight_kg, mean)) %>%
  ggplot(aes(x = country, y = weight_kg)) +
  geom_jitter(
    alpha = 0.75,
    size = 3,
    position = position_jitter(0.2),
    aes(color = str_to_title(position))
  ) +
  coord_flip() +
  geom_flag(
    data = f,
    aes(x = country, y = 60, country = country),
    size = 8
  ) +
  paletteer::scale_color_paletteer_d(
    "ggthemes::wsj_rgby",
    guide = guide_legend(
      label.position = "bottom",
      direction = "horizontal",
      override.aes = list(size = 4)
    )
  ) +
  scale_y_continuous(
    name = "Weight (kg)",
    breaks = scales::breaks_pretty()
  ) +
  scale_x_discrete(
    name = NULL,
    labels = function(x) {
      countrycode::countrycode(x, "iso2c", "country.name")
    }
  ) +
  labs(
    title = "Player weights of the IIHF 2020",
    subtitle = "Teams are ordered by the average weight",
    caption = "Data from https://www.tsn.ca/ and https://www.hockeycanada.ca/ | @philmassicotte"
  ) +
  theme(
    panel.background = element_rect(fill = "#3c3c3c"),
    plot.background = element_rect(fill = "#3c3c3c"),
    legend.background = element_rect(fill = "#3c3c3c"),
    legend.key = element_rect(fill = "#3c3c3c"),
    legend.position = "bottom",
    legend.spacing.x = unit(1, "cm"),
    legend.title = element_blank(),
    # legend.text = element_text(margin = margin(t = 1, r = 10, b = 1, l = 10)),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white", hjust = 0.5),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_line(size = 0.1, color = "gray50"),
    panel.grid.major.y = element_line(size = 0.1, color = "gray50"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(color = "gray75", hjust = 0.5),
    plot.caption = element_text(color = "gray75", hjust = 1, family = "Maven Pro", size = 8)
  )

ggsave(
  here::here("graphs", "2020_world_juniors.png"),
  type = "cairo",
  device = "png",
  width = 7,
  height = 7 / 1.16,
  dpi = 600
)

df %>%
  write_csv(here::here("data/clean", "2020_world_juniors_rosters.csv"))
