#horizontal ve vertical facet yapmak öğrenicez
#örneğin hocanın verdiği derse göre ayrı facet 
#cinsiyetine göre ayrı facet etti sana 4 grafik tek sayfada

library(tidyverse)
library(car)
library(forcats)

#temayı baştan belirledik.

theme_set(theme_bw()+
            theme(title = element_text(colour = "steelblue",
                                       face = "bold")))

Salaries %>% 
  mutate(discipline = factor(discipline,
                             levels = c("A","B"),
                             labels= c("Agriculture",
                                       "Biology"))) %>% 
  ggplot(aes(yrs.since.phd,salary))+
  geom_point(aes(color=rank), alpha = 0.5)+
  geom_smooth()+
  facet_grid(sex~discipline)+
  labs(title = "Salary vs years since PhD",
       x="Number of year since PhD",
       y="Current Salary",
       color="Position")
