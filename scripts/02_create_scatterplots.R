library(tidyverse)
library(ggrepel)
library(haven)
library(dslabs)
library (ggplot2)

auburn_orange <- '#F66733'
auburn_blue <- '#0021A5'

ed <- read_sheet('https://docs.google.com/spreadsheets/d/1DBve4JLWO-tCRHOF8aTBYVG_T5ndqQikje37zUBLCMU/edit#gid=0')

ed

ed %>%
  ggplot(aes(x = log2(municipality_population), y = football_stadium_capacity, color = acc_accreditation_or_certification)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('yes' = auburn_blue, 'no' = auburn_orange),
                     breaks = c('yes', 'no'),
                     labels = c('Accredited', 'Not Accredited')) +
  geom_label_repel(aes(label = school), max.overlaps = 100, size = 2, box.padding = 1) +
  labs(x = 'Municipality Population', y = 'Football Stadium Capacity', color = 'ACC Accreditation or Certification',
       title = 'Scatterplot of Municipality Population vs Football Stadium Capacity',
       subtitle = 'In Relation to ACC Accreditation or Certification') +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(shape = c(16))))

ed %>%
  ggplot(aes(x = log2(municipality_population), y = football_stadium_capacity, color = academic_hospital_in_municipality)) +
  geom_point(size = 3) +
  scale_color_manual(values = c('yes' = auburn_blue, 'no' = auburn_orange),
                     breaks = c('yes', 'no'),
                     labels = c('Yes', 'No')) +
  geom_label_repel(aes(label = school), max.overlaps = 100, size = 2, box.padding = 1) +
  labs(x = 'Municipality Population', y = 'Football Stadium Capacity', color = 'Presence of Academic Hospital in Municipality',
       title = 'Scatterplot of Municipality Population vs Football Stadium Capacity',
       subtitle = 'In Relation to the Presence of an Academic Hospital in Municipality') +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(shape = c(16))))

ed %>%
  ggplot(aes(x = log2(distance_nearest_emergency_department_miles), y = football_stadium_capacity)) +
  geom_point(size = 3, color = auburn_blue) +
  geom_label_repel(aes(label = school), max.overlaps = 100, size = 2, box.padding = 1, color = auburn_blue) +
  labs(x = 'Distance to Nearest Emergency Department', y = 'Football Stadium Capacity',
       title = 'Scatterplot of Distance to the Nearest Emergency Department vs Football Stadium Capacity') +
  theme_minimal()

p <- ed %>%
  ggplot(aes(x = log2(closest_ed_number_beds), y = football_stadium_capacity)) +
  geom_point(size = 3, color = auburn_blue) +
  geom_label_repel(aes(label = school), max.overlaps = 100, size = 2, box.padding = 1, color = auburn_blue) +
  labs(x = 'Closest Emergency Department Number of Beds', y = 'Football Stadium Capacity',
       title = 'Scatterplot of the Number of Beds in the Closest Emergency Department vs Football Stadium Capacity') +
  theme_minimal()

ggsave('Distance to Nearest ED vs Football Stadium Capacity.pdf', p, width = 14, height = 6, units = 'in')
