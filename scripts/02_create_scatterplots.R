library(tidyverse)
library(ggrepel)
library(haven)
library (ggplot2)

auburn_orange <- '#F66733'
auburn_blue <- '#0021A5'

ed <- read_rds("../data/clean/01_clean_dataset.rds")

ed

ed %>%
  ggplot(aes(x = log2(municipality_population), y = football_stadium_capacity, color = acc_accreditation_or_certification)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('yes' = auburn_blue, 'no' = auburn_orange),
                     breaks = c('yes', 'no'),
                     labels = c('Accredited', 'Not Accredited')) +
  geom_label_repel(aes(label = school), max.overlaps = 100, size = 3, box.padding = 1, alpha = 0.25) +
  labs(x = 'log2(Municipality Population)', y = 'Football Stadium Capacity', color = 'ACC Accreditation or Certification',
       title = 'Scatterplot of Municipality Population vs Football Stadium Capacity',
       subtitle = 'In Relation to ACC Accreditation or Certification') +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(shape = c(16))))

ed %>%
  ggplot(aes(x = log2(municipality_population), y = football_stadium_capacity, color = factor(teaching_hospital_in_municipality) %>% fct_recode("yes" = "1", "no" = "0"))) +
  geom_point(size = 3) +
  scale_color_manual(values = c('yes' = auburn_blue, 'no' = auburn_orange),
                     breaks = c('yes', 'no'),
                     labels = c('Yes', 'No')) +
  geom_label_repel(aes(label = school), max.overlaps = 100, size = 3, box.padding = 1, alpha = 0.25, show.legend = FALSE) +
  labs(x = 'log2(Municipality Population)', y = 'Football Stadium Capacity', color = 'Presence of Teaching Hospital in Municipality',
       title = 'Scatterplot of Municipality Population vs Football Stadium Capacity',
       subtitle = 'In Relation to the Presence of an Academic Hospital in Municipality') +
  theme_minimal()

f <- ed %>%
  ggplot(aes(x = distance_nearest_emergency_department_miles, y = football_stadium_capacity)) +
  geom_point(size = 3, color = auburn_blue) +
  geom_label_repel(aes(label = school), max.overlaps = 100, size = 3, box.padding = 1, color = auburn_blue, alpha = 0.25) +
  labs(x = 'Distance to Nearest Emergency Department', y = 'Football Stadium Capacity',
       title = 'Scatterplot of Football Stadium Capacity vs Distance to the Nearest Emergency Department') +
  theme_minimal()

p <- ed %>%
  ggplot(aes(x = closest_ed_number_beds, y = football_stadium_capacity)) +
  geom_point(size = 3) +
  geom_label_repel(aes(label = school), max.overlaps = 100, size = 3, box.padding = 1, label.r = 0, alpha = 0.25) +
  labs(x = 'Closest Emergency Department Number of Beds', y = 'Football Stadium Capacity',
       title = 'Scatterplot of Football Stadium Capacity vs the Number of Beds in the Closest Emergency Department') +
  theme_minimal()

ggsave('../figs/Closest ED number of beds vs Football Stadium Capacity.pdf',p, width = 14, height = 6, units = 'in')

ggsave('../figs/Distance to Nearest ED vs. Football Stadium Capacity.pdf',f, width = 14, height = 6, units = 'in')

p

