library(tidyverse)
library(ggpmthemes)
library(ggtext)

theme_set(theme_light_modified(base_family = "Oxanium"))

Sys.setlocale("LC_TIME", "fr_CA.UTF-8")

# https://ici.radio-canada.ca/nouvelle/1713466/canada-surmortalie-deces-morts-covid-coronavirus-pandemie

file <- pins::pin("https://www.stat.gouv.qc.ca/statistiques/population-demographie/deces-mortalite/DecesSemaine_QC_2010-2020_GrAge.xlsx")

df <- readxl::read_excel(file, skip = 5) %>%
  drop_na(Statut) %>%
  type_convert() %>%
  pivot_longer(
    cols = matches("\\d"),
    names_to = "semaine",
    values_to = "deces"
  ) %>%
  janitor::clean_names() %>%
  filter(groupe_dage != "Total") %>%
  type_convert() %>%
  drop_na(deces)

df <- df %>%
  filter(semaine < 53) %>%
  mutate(date_true = as.Date(paste(annee, semaine, 1,  sep = "-"), "%Y-%W-%w")) %>%
  mutate(date = as.Date(paste("2012", semaine, 1,  sep = "-"), "%Y-%U-%u")) %>%
  mutate(mois = lubridate::month(date, label = TRUE)) %>%
  mutate(date = as.Date(date))

df_2020 <- df %>%
  filter(annee == 2020)

p <- df %>%
  drop_na(deces) %>%
  mutate(annee = as.factor(annee)) %>%
  ggplot(aes(x = date, y = deces, group = annee)) +
  geom_line(color = "gray75", size = 0.15) +
  geom_line(data = df_2020, color = "#ef4040", size = 0.5) +
  facet_wrap(~groupe_dage, scales = "free_y", ncol = 1) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 4),
    labels = scales::label_number()
  ) +
  labs(
    y = "Nombre de décès",
    x = NULL,
    title = str_wrap("Nombre de décès par semaine au Québec entre 2010 et 2020", 35),
    subtitle = "La ligne <span style = 'color:#ef4040;'>rouge</span> représente les décès de 2020 alors que les lignes grises représentent les<br>années antérieures.",
    caption = glue::glue("Source : Institut de la statistique du Québec, fichier des décès du Registre des événements démographiques du Québec.\nDonnées extraites le {max(df_2020$date_true)}.")
  ) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill = "#3c3c3c"),
    strip.text = element_text(face = "bold", hjust = 0, color = "white"),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_markdown(size = 8, lineheight = 1.5),
    plot.caption = element_text(size = 5)
  )

ggsave(
  here::here("graphs/", "deces_quebec_surmortalite.png"),
  width = 6,
  height = 6,
  dpi = 600
)
