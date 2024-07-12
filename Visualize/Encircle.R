library(tidyverse)
library(ggalt)
theme_set(theme_minimal())#it applies all of the plots we create  bi bakarsın buna yine
View(starwars)
just_humans=starwars%>%
  filter(species=="Human")#Sadece humanları al
starwars%>%
  filter(mass<200)%>%
drop_na(sex)%>%
  ggplot(aes(height,mass,colour=sex))+
  geom_point()+
  geom_encircle(data=just_humans,
                colour="steelblue",#opsionel
                size=2,#opsiyonel
                alpha=0.8, #opsiyonel
                expand=0.1)#opsionel
#En basit haliyle just humansı yuvarlak içine aldık.


