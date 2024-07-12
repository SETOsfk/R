#Analyze

#t-test(tek örnek)
#t test(two sided)
#t test(one sided)
#t test(paired)



library(gapminder)
library(tidyverse)
library(patchwork)
View(gapminder)


#Hipotez testi tek örnek :
#Afrikadaki ortalama yaşam süresini 48.9 bulduk tamam öyle de
#Bu ortalamaya güvenebilir miyiz 
#H0: life expectancy in africa =50
#h1: life expectancy !=50 
#Obs:
#Sample data provides avg lifeexp is 48.9
#is this statistically significant??

ttest50=gapminder%>%
  filter(continent=="Africa")%>%
  select(lifeExp)%>%
  t.test(mu=50)#mu 50ye eşit mi t.testi yap
ttest50

attributes(ttest50)#bunun içinde ne olduğunnu gösterir
ttest50$p.value
#confidence interval da 50yi içermiyor 
#bu 

#TWO SIDED T TEST for difference of means
#eu ve afrika arasındaki lifeExp farkı test edicez

gapminder%>%
  filter(continent %in% c("Africa","Europe"))%>%
  t.test(lifeExp~continent,data=. ,
         alternative="two.sided")
#H0 afrika ve eudaki life expect aynıdır =
#h1 aynı değildir. !=




#one sided diff in means
#daha az, daha fazla gibi şeyler.
gapminder%>%
  filter(country %in%c("Ireland","Switzerland"))%>%
  t.test(lifeExp ~ country,data=. ,
         alternative="less",
         conf.level=0.95)
#irelanddaki life exp switzerlanddan daha mı az?

#PAIRED T TEST
#Bir popülasyonu alıyoruz afrikadaki her ülke
#bir life exp var 1957 ve 2007 arasındaki farka bakıcaz

gapminder%>%
  filter(year %in% c(1957,2007)&
           continent=="Africa")%>%
  mutate(year=factor(year,levels = c(2007,1957
            )))%>%
  t.test(lifeExp~year,data=. ,
         paired=TRUE)
#T testinin varsayımları
#1)Aynı varsayıma sahip olan örneklemler
#2)Elle tutulabilir seviyede örneklem
#3)Normal dağılıma sahip olmak


#https://www.youtube.com/watch?v=fO2X-8FXY6k&t=13s&ab_channel=RProgramming101
















