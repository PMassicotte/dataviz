library(tidyverse)
library(pdftools)
library(tabulizer)
library(pins)
library(gender)
library(furrr)
library(ggpmthemes)
library(ggtext)

theme_set(theme_light_modified(base_family = "Maven Pro"))

plan(multiprocess(workers = availableCores() - 1))

url <-
  pin("http://arcticnetmeetings.ca/docs/programasm2019-web.pdf")

df <- extract_tables(url, pages = 9:29, encoding = "UTF-8")

res <- map_at(df, 1, ~..1) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate_if(is.factor, as.character)

res <- res %>%
  filter(nchar(V1) > 1) %>%
  pull(V2) %>%
  enframe(name = NULL, value = "fullname") %>%
  extract(
    fullname,
    into = c("firstname", "lastname"),
    regex = "(.*)\\W(.*)",
    remove = FALSE
  ) %>%
  mutate(gender = future_pmap(., ~gender(..2), .progress = TRUE))

df_viz <- res %>%
  mutate(n = map_int(gender, nrow)) %>%
  filter(n == 1) %>%
  unnest(gender)

# Plot --------------------------------------------------------------------

df_viz %>%
  count(gender) %>%
  mutate(gender = str_to_title(gender)) %>%
  mutate(gender = fct_reorder(gender, n)) %>%
  ggplot(aes(x = gender, y = n, fill = gender)) +
  geom_col() +
  coord_flip() +
  xlab(NULL) +
  ylab("Number of presenter") +
  labs(
    title = str_wrap("Gender distribution of the speakers of the conference Arcticnet 2019", 40),
    caption = "Analysis performed using the **ArcticNet ASM2019 full programme**<br>arcticnetmeetings.ca/docs/programasm2019-web.pdf"

  ) +
  paletteer::scale_fill_paletteer_d(ggthemes, wsj_rgby) +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(color = "gray70", hjust = 0.5, size = 14),
    plot.caption = element_markdown(color = "gray60", size = 7, hjust = 0.5)
  )

ggsave(
  here::here("graphs", "arctinet_2019_gender.png"),
  type = "cairo",
  device = "png",
  dpi = 600
)
