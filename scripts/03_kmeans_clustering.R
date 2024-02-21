library(tidyverse)
library(ggrepel)

set.seed(5454)

df <- read_rds("../data/clean/01_clean_dataset.rds")

df_full_dataset_no_ed_beds <- df %>%
  select(municipality_population, 
         football_stadium_capacity,
         distance_nearest_emergency_department_miles,
         teaching_hospital_in_municipality,
         is_closest_ed_free_standing,
         acc_accreditation_or_certification,
         primary_care_physicians
         ) %>%
  mutate(across(where(is.numeric), ~scale(.)[, 1])) %>%
  mutate(across(where(is.character), ~if_else(. == "yes", 1, 0))) %>%
  filter(is.na(primary_care_physicians) == FALSE)


df_full_dataset_no_ed_beds_m <- as.matrix(df_full_dataset_no_ed_beds)

k.means.3 <- kmeans(df_full_dataset_no_ed_beds_m, centers = 3, nstart = 20)

clusters_3_no_ed_beds <- df %>%
  mutate(cluster = k.means.3$cluster) %>%
  select(school, cluster)

write_rds(clusters_3_no_ed_beds, "../output/03_clusters_3_no_ed_beds.rds")

df %>%
  mutate(cluster = factor(k.means.3$cluster)) %>%
  ggplot(aes(x = log2(municipality_population),
             y = football_stadium_capacity,
             color = cluster)) +
  geom_point() +
  geom_label_repel(aes(label = school))

k.means.4 <- kmeans(df_full_dataset_no_ed_beds_m, centers = 4, nstart = 20)  

clusters_4_no_ed_beds <- df %>%
  mutate(cluster = k.means.4$cluster) %>%
  select(school, cluster)

write_rds(clusters_4_no_ed_beds, "../output/03_clusters_4_no_ed_beds.rds")

p <- df %>%
  mutate(cluster = factor(k.means.4$cluster)) %>%
  ggplot(aes(x = log2(municipality_population),
             y = football_stadium_capacity,
             color = cluster)) +
  geom_point() +
  geom_label_repel(aes(label = school), size = 3, show.legend = FALSE) +
  ggthemes::theme_tufte() +
  scale_color_manual(values = c("#0b2341", "#e86100", "#006c9a", "#215834"),
                     name = "Cluster assignment from\nKmeans algorithm") +
  labs(
    y = "Football stadium capacity",
    x = "Log2(municipal population)"
  ) +
  theme(legend.position = "bottom")

p

ggsave("../figs/03_kmeans_4_scatterplot.pdf", p, width = 14, height = 7, units = "in")

k.means.5 <- kmeans(df_full_dataset_no_ed_beds_m, centers = 5, nstart = 20) 

clusters_5_no_ed_beds <- df %>%
  mutate(cluster = k.means.5$cluster) %>%
  select(school, cluster)

write_rds(clusters_5_no_ed_beds, "../output/03_clusters_5_no_ed_beds.rds")

df %>%
  mutate(cluster = factor(k.means.5$cluster)) %>%
  ggplot(aes(x = log2(municipality_population),
             y = football_stadium_capacity,
             color = cluster)) +
  geom_point() +
  geom_label_repel(aes(label = school))
