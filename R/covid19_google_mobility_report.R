library(tidyverse)

# https://github.com/datasciencecampus/mobility-report-data-extractor
# https://drive.google.com/drive/folders/1VFkLwK3vXm96ZXWv7HzSrL9aisE_0nJD

df <- tibble::tribble(
   ~value,        ~date,  ~origin,
   40.662, "2020-02-16", "Canada",
   62.839, "2020-02-17", "Canada",
    6.031, "2020-02-18", "Canada",
     9.33, "2020-02-19", "Canada",
    13.55, "2020-02-20", "Canada",
   10.242, "2020-02-21", "Canada",
   27.859, "2020-02-22", "Canada",
   36.014, "2020-02-23", "Canada",
   18.663, "2020-02-24", "Canada",
    8.977, "2020-02-25", "Canada",
    0.646, "2020-02-26", "Canada",
   -5.982, "2020-02-27", "Canada",
   -5.097, "2020-02-28", "Canada",
   10.508, "2020-02-29", "Canada",
   22.956, "2020-03-01", "Canada",
    1.032, "2020-03-02", "Canada",
    8.412, "2020-03-03", "Canada",
   10.917, "2020-03-04", "Canada",
   17.533, "2020-03-05", "Canada",
    5.614, "2020-03-06", "Canada",
   17.219, "2020-03-07", "Canada",
   49.772, "2020-03-08", "Canada",
   30.748, "2020-03-09", "Canada",
   -2.055, "2020-03-10", "Canada",
   11.658, "2020-03-11", "Canada",
   11.479, "2020-03-12", "Canada",
  -16.249, "2020-03-13", "Canada",
   -8.151, "2020-03-14", "Canada",
   27.697, "2020-03-15", "Canada",
   14.783, "2020-03-16", "Canada",
    6.772, "2020-03-17", "Canada",
   14.221, "2020-03-18", "Canada",
   17.924, "2020-03-19", "Canada",
    -7.84, "2020-03-20", "Canada",
   11.465, "2020-03-21", "Canada",
    3.991, "2020-03-22", "Canada",
  -26.686, "2020-03-23", "Canada",
   -9.855, "2020-03-24", "Canada",
    1.335, "2020-03-25", "Canada",
  -17.949, "2020-03-26", "Canada",
   -9.585, "2020-03-27", "Canada",
  -30.058, "2020-03-28", "Canada",
  -16.339, "2020-03-29", "Canada"
  )


df %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line()
