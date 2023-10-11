qsr <- read.csv('QSR.csv') ## Change this line according to the location of your data file 

library(tidyverse)
library(ggthemes)
#library(viridis)
names(qsr)

## Data Prep

## Pre-Post scores for all centers

qsr <- qsr %>%
        mutate('Center_new' = 
                 str_replace(Center,
                             pattern = '^QSR.*',
                             replacement = 'QSR')) %>%
        mutate('Pre' = str_replace_all(PreAppointment_Feeling,
                                       pattern = '[^12345]',
                                       replacement = '')) %>%
        mutate('Pre' = as.numeric(Pre)) %>%
        mutate('Post' = str_replace_all(PostAppointment_Feeling,
                                        pattern = '[^12345]',
                                        replacement = '')) %>%
        mutate('Post' = as.numeric(Post)) %>%
        mutate('Pre_Feeling' = ifelse(PreAppointment_Feeling
                                 %in% c('(1) Super uncertain',
                                        '(2) Uncertain'),
                                 'Low',
                                 ifelse(PreAppointment_Feeling
                                        %in% c('(3) Neutral'),
                                        'Medium',
                                        'High'))) %>%
        mutate('Feeling_Difference' = Post - Pre)



## Setting the levels for the Pre feeling buckets
qsr$Pre_Feeling <- factor(qsr$Pre_Feeling)
levels(qsr$Pre_Feeling) <- c('Low', 'Medium', 'High')


## Example Plot

qsr %>%
  filter(str_detect(string = Center,
                    pattern = 'QSR')) %>%
  filter(str_detect(string = Center,
                    pattern = 'QSR Facil')) %>%
  group_by(Center, Faculty) %>%
  summarise('Frequency' = n() ) %>%
  arrange(-Frequency) %>%
  head(n = 20) %>%
  ggplot() +
  aes(x = reorder(Faculty, Frequency) ,
      y = Frequency) +
  
  geom_bar(stat = 'identity', fill = 'blue') +
  coord_flip()


### Feeling Difference for all centers
qsr %>%
  filter(Feeling_Difference > -4) %>%
  filter(Feeling_Difference < 4) %>%
  filter(Feeling_Difference != 0) %>%
  ggplot() +
       aes(x = Pre_Feeling,
           y = Feeling_Difference,
           color = Pre_Feeling) +
  geom_jitter(width = 0.1,
              height = 0.2,
              alpha = 0.5) +
  facet_grid( . ~ Center_new) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = 'bottom') +
  scale_color_canva(palette = names(canva_palettes)[2])
  #scale_color_brewer(palette = 'Set2')
  

