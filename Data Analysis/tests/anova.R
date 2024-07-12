#İki mean için t.test eğer iki kıtayı karşılaştıracak olsak
#Ancak 3. kıta işin içine girince t.test yapamazsın

library(tidyverse)
library(gapminder)
library(forcats)
library(patchwork)
view(msleep)
View(gapminder)

my_data=msleep%>%
  select(vore,sleep_rem)%>%
  drop_na()#Na verileri atarak vore ve sleep rem collarını seç

mod1=aov(sleep_rem~vore,data=my_data)#sleep_rem ve vore arasında anova yap
summary(mod1)
#P 0.05 ten küçükse H0 Red 
#H1 kabul means REM SÜRELERİ FARKLI

#pipe ile
msleep%>% 
  select(vore,sleep_rem)%>%
  drop_na()%>%
  aov(sleep_rem~vore,data = .)%>%
  summary()

gapdata=gapminder%>%
  filter(year==2007 &
           continent %in% c("Americas","Europe","Asia")) %>%
  select(continent,lifeExp)
#Ortalamalrın dağılımlarına bak
gapdata%>%
  group_by(continent)%>%
  summarise(mean_life=mean(lifeExp))%>%
  arrange(mean_life)
#Question: Is the life expectancy
#          in these 3 continents different?

#Hipotez testi:  H0:Mean life expectancy is the same
#                H1: Is not the same

#Gözlem:        Difference in mean is observed
#               in the sample data, but is this 
#                statistically significant(alpha=0.05)

#Create anova model
gapdata%>%
   aov(lifeExp~continent,data=.)%>%
   summary
aov_model=gapdata%>%
  aov(lifeExp~continent,data=.)%>%
  summary#America avrupa asya arasında beklenen yaşam dağılımına baktık.
#Bunlarından birisi diğer ikisinden farklı dedik ama hangisi olduğunu
#söylemedik
gapdata%>%
  aov(lifeExp~continent,data=.)%>%
  TukeyHSD()%>%
  plot()

#Tukey testinde de görüldüğü gibi (plotı silip bak)
#Diff between Asia and Americas
#has an adjusted p value of 0.14(not significant)




























