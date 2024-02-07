library(tidyverse)
library(ggrepel)
library(haven)
library(dslabs)

auburn_blue <- '#0021A5'
auburn_orange <- '#F66733'

df <- read_sheet('https://docs.google.com/spreadsheets/d/1DBve4JLWO-tCRHOF8aTBYVG_T5ndqQikje37zUBLCMU/edit#gid=0')

df

df %>%
  ggplot(aes(x= log2(municipality_population), y= football_stadium_capacity, color= academic_hospital_in_municipality)) +
  geom_point(size= 3)+
  scale_color_manual( values = c( 'yes'= auburn_blue, 'no'= auburn_orange), labels= c ('yes'= 'Academic', 'no'= 'Not Academic')) +
  geom_label_repel(aes(label= school), max.overlaps = 100, size = 1, box.padding = 1, label.r = 0) +
  theme_minimal() +
  labs( x= 'Municipality Population', y= 'Football Stadium Capacity') + 
  ggtitle( 'Scatterplot of Municipality Population v. Stadium Capacity', 
           subtitle = 'In relation to Academic Hospitals') +
  guides(color= guide_legend(title = "Academic Hosptials", override.aes = list(shape = 10)))
