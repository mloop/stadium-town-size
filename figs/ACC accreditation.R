library(tidyverse)
library(ggrepel)
library(haven)
library(dslabs)

auburn_blue <- '#0021A5'
auburn_orange <- '#F66733'

df <- read_sheet('https://docs.google.com/spreadsheets/d/1DBve4JLWO-tCRHOF8aTBYVG_T5ndqQikje37zUBLCMU/edit#gid=0')

df

df %>%
  ggplot(aes(x= log2(municipality_population), y= football_stadium_capacity, color= acc_accreditation_or_certification)) +
  geom_point(size= 3)+
  scale_color_manual( values = c( 'yes'= auburn_blue, 'no'= auburn_orange), labels= c ('yes'= 'Accredited', 'no'= 'Not Accredited')) +
  geom_label_repel(aes(label= school), max.overlaps = 100, size = 1, box.padding = 1, label.r = 0) +
  theme_minimal() +
  labs( x= 'Municipality Population', y= 'Football Stadium Capacity') + 
  ggtitle( 'Scatterplot of Municipality Population v. Stadium Capacity', 
           subtitle = 'In relation to ACC Accreditation or Certification') +
  guides(color= guide_legend(title = "ACC Accreditatoin", override.aes = list(shape = 10)))
