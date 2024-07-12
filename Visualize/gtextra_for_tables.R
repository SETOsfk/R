pacman::p_load(gtExtras)
library(tidyverse)
#Create a summary of your data

iris %>% 
  gt_plt_summary()


#Inster icons and graphics into tables
mtcars %>% 
  group_by(cyl) %>% 
  summarize(Median = round(median(mpg),1), #sayıyı yuvarladık bir decimal
            Mean= round(mean(mpg),1),
            Distribution = list(mpg)) %>% 
  gt() %>% 
  gt_plt_dist(Distribution) %>% 
  gt_theme_guardian() %>% 
  tab_header(title = "Miles Per Gallon Stats")
  
