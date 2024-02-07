library(tidyverse)
library(ggrepel)
library(haven)
library(dslabs)
library(googlesheets4)

auburn_blue <- '#0021A5'
auburn_orange <- '#F66733'

df <- read_sheet('https://docs.google.com/spreadsheets/d/1DBve4JLWO-tCRHOF8aTBYVG_T5ndqQikje37zUBLCMU/edit#gid=0')

df

p <- df %>%
  ggplot(aes(x= log2(municipality_population), y= football_stadium_capacity, color= conference )) +
  geom_point(size= 3)+
  scale_color_manual( values = c( 'SEC'= '#0021A5','Pac-12'= '#F66733', 'Big 10' = '#2CA02C', 'Big 12' = '#D62728', 'ACC' = '#9467BD', 'Independent' = '#8C564B')) +
  geom_label_repel(aes(label= school), max.overlaps = 100, size = 1, box.padding = 1, label.r = 0) +
  theme_minimal() +
  labs( x= 'Municipality Population', y= 'Football Stadium Capacity') + 
  ggtitle( 'Scatterplot of Municipality Population v. Stadium Capacity', 
           subtitle = 'Conference') +
  guides(color= guide_legend(title = "Conference", override.aes = list(shape = 10)))

p

ggsave('Football Stadium Capcacity v. Municippality Population; Conference.pdf', p, width = 14, height = 6, units = 'in')

