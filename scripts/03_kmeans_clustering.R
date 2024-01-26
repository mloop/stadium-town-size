library(tidyverse)
library(ggrepel)

df <- read_rds("../data/clean/01_clean_dataset.rds")

df_full_dataset_no_ed_beds <- df %>%
  select(municipality_population, 
         football_stadium_capacity,
         distance_nearest_emergency_department_miles,
         academic_hospital_in_municipality,
         is_closest_ed_free_standing,
         # this is missing right now acc_chest_pain_accreditation_or_certification
         ) %>%
  mutate(across(where(is.numeric), ~scale(.)[, 1])) %>%
  mutate(across(where(is.character), ~if_else(. == "yes", 1, 0)))


df_full_dataset_no_ed_beds_m <- as.matrix(df_full_dataset_no_ed_beds)

k.means.3 <- kmeans(df_full_dataset_no_ed_beds_m, centers = 3, nstart = 20)         

df %>%
  mutate(cluster = factor(k.means.3$cluster)) %>%
  ggplot(aes(x = log2(municipality_population),
             y = football_stadium_capacity,
             color = cluster)) +
  geom_point() +
  geom_label_repel(aes(label = school))

k.means.4 <- kmeans(df_full_dataset_no_ed_beds_m, centers = 4, nstart = 20)         

p <- df %>%
  mutate(cluster = factor(k.means.4$cluster)) %>%
  ggplot(aes(x = log2(municipality_population),
             y = football_stadium_capacity,
             color = cluster)) +
  geom_point() +
  geom_label_repel(aes(label = school))

p

ggsave("../figs/03_kmeans_4_scatterplot.png", p, width = 12, height = 7, units = "in")

k.means.5 <- kmeans(df_full_dataset_no_ed_beds_m, centers = 5, nstart = 20)         

df %>%
  mutate(cluster = factor(k.means.5$cluster)) %>%
  ggplot(aes(x = log2(municipality_population),
             y = football_stadium_capacity,
             color = cluster)) +
  geom_point() +
  geom_label_repel(aes(label = school))
