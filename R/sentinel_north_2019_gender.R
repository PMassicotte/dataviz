library(tidyverse)
library(pdftools)
library(ggpmthemes)

theme_set(theme_exo())

program <- pdf_text("https://sentinellenord.ulaval.ca/sites/default/files/2019-08/RSSN2019%20Programme%20final.pdf")

res <- program %>%
  str_match_all("•  (.*) –|-- (.*) –") %>%
  unlist() %>%
  na.omit() %>%
  enframe(name = NULL) %>%
  filter(str_detect(value, "^[:alpha:]")) %>%
  extract(value, into = c("firstname", "lastname"), regex = "^(\\w+)\\s?(.*)$") %>%
  mutate(firstname = stringi::stri_trans_general(firstname, "Latin-ASCII")) %>%
  mutate(gender = pmap(., ~gender::gender(..1))) %>%
  mutate(found = map_int(gender, nrow)) %>%
  filter(found == 1) %>%
  unnest(gender)

res


# Plot --------------------------------------------------------------------

p <- res %>%
  count(gender) %>%
  mutate(proportion = n / sum(n)) %>%
  mutate(gender = str_to_title(gender)) %>%
  ggplot(aes(x = gender, y = proportion)) +
  geom_col() +
  geom_text(aes(label = paste(round(proportion, 2) * 100, "%"), hjust = 1.1), color = "white") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, expand = expand_scale(c(0, 0.2))) +
  xlab(NULL) +
  ylab("Proportion of speakers") +
  labs(
    title = str_wrap("Proportion of speaker gender at the 2019 Sentinel North Scientific Meeting", 30),
    subtitle = "Text analysis based on the conference program",
    caption = "Data: https://sentinellenord.ulaval.ca/programmeRSSN2019pdf"
  ) +
  theme(
    plot.caption = element_text(color = "gray50"),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

p

ggsave(
  here::here("graphs", "sentinel_north_2019_gender.png"),
  type = "cairo",
  device = "png",
  dpi = 300
)


