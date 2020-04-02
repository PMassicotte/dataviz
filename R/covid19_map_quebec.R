library(tidyverse)
library(rvest)
library(sf)
library(rcartocolor)
library(ggpmthemes)
library(ggforce)

theme_set(theme_maven())

url <-
  "https://www.quebec.ca/sante/problemes-de-sante/a-z/coronavirus-2019/situation-coronavirus-quebec/"

df <- read_html(url) %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table() %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  filter(str_detect(regions, "^\\d{2}")) %>%
  type_convert() %>%
  extract(regions,
    into = c("region_no", "region_nom"),
    regex = "(\\d{2})\\W+(.*)"
  ) %>%
  rename(n = 3) %>%
  mutate(n = str_remove_all(n, "\\W+")) %>%
  mutate(n = parse_number(n))

df

# https://www.donneesquebec.ca/recherche/fr/dataset/decoupages-administratifs
# https://publications.msss.gouv.qc.ca/msss/document-001647/
region_sf <-
  st_read("data/raw/Territoires_RSS_2020/Territoires_RSS_2020.shp")

region_sf

df_viz <- df %>%
  full_join(region_sf %>% as_tibble(), by = c("region_no" = "RSS_code")) %>%
  st_as_sf() %>%
  st_transform(crs = 3799) %>%
  group_by(region_nom) %>%
  summarise(n = mean(n))

lab <- df_viz %>%
  mutate(center = st_centroid(geometry)) %>%
  drop_na(n) %>%
  as_tibble() %>%
  cbind(st_coordinates(.$center))

lab

df_viz %>%
  ggplot() +
  geom_sf(aes(fill = n),
    show.legend = FALSE,
    size = 0.25,
    color = "gray75"
  ) +
  scale_fill_carto_c(palette = "SunsetDark") +
  ggrepel::geom_text_repel(
    data = lab,
    aes(
      x = X,
      y = Y,
      label = glue::glue("{str_wrap(region_nom, 10)} ({n})"),
    ),
    size = 3,
    hjust = 0.5,
    vjust = 0.5,
    box.padding = 0.7,
    point.padding = 0.5
  ) +
  # geom_mark_circle(
  #   data = lab,
  #   aes(
  #     x = X,
  #     y = Y,
  #     label = str_wrap(region_nom, 10),
  #     description = glue::glue("Cas confirmés: {n}"),
  #     group = region_no
  #   ),
  #   label.fontsize = 10,
  #   label.buffer = unit(0.01, "cm"),
  #   expand = unit(0.1, "cm"),
  #   label.fill = "transparent",
  #   con.size = 0.25,
  #   con.colour = "gray65",
  #   con.border = "none"
  # ) +
  coord_sf() +
  labs(
    title = str_wrap("Nombre de cas de coronavirus confirmés au Québec", 40),
    subtitle = glue::glue("{sum(lab$n)} cas en date du {Sys.Date()}"),
    caption = "Données: https://www.quebec.ca/sante/problemes-de-sante/a-z/coronavirus-2019/"
  ) +
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.title.position = "plot"
  )

ggsave(
  here::here("graphs", "covid19_map_quebec.png"),
  type = "cairo",
  dpi = 600,
  width = 8,
  height = 8
)
