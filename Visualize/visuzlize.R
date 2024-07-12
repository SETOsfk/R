#Visualize your data

#Tell your story with data:
#Görselleştirirsen çok daha iyi anlaşılıe herkes tarafından
#Types of combinations of data: Neyi neyle kombinleyeceksin numeric-numericse hangi 
#grafik vb
#The grammar of graphics.
  #Data
   #Mapping
   #Geometry

#Ne zaman hangi grafiği kullanacaksın?

#Sadece bir numeric veride, 
#histogram kullanabilirsin, ya da density plot
#boxplot ya da violin plot-birazcık boxplota benziyor, kategorik veri tarafından
#ayrıştırılmış veriye bakarken daha işine yarar-.

#Bir ya da+ kategorik veri,
#1 kategorik veriye bakıyorsan barplot
#diğer bir kategori eklersen barplotını bölebilirsin ya da
#grouped barplot,percentage barplot da olabilir.

#2 kategorik 1 numericse, 
#Density plot, boxplot

#2 numeric 1 kategorik
#2 numeric scatter plot kategoriği de ekleyebiliriz
#farklı renk atayarak.


library(tidyverse)
library(gapminder)
#Grafiği seçmek

starwars
gapminder

gapminder%>%
  filter(continent %in% c("Africa","Europe"))%>% #continentin africa ve eu içerenlerini dahil et
  filter(gdpPercap<30000)%>% #gdppercap 30kdan az olsun
  ggplot(aes(gdpPercap,#x'in
             lifeExp,#y in
             size=pop,#boyutun
             color=year))+#rengin
  geom_point(colour="steelblue")+#grafik tipin
  facet_wrap(~continent)+#kıtalara göre iki farklı grafik oluştur, 
  #ncol fonskyionun girmedik çünkü zaten kıtaları yukarda filter ile seçmiştik 
  #grafiği af ve eu olarak bölücek
  labs(title="Life expectancy explained by GDP",#başlığın
       x="GDP per capita",#x kısmı neyi gösterir
       y="Life expectancy")#y kısmı neyi gösterir.
 #En basic şekilde afrika ve eu kıtalarında
#yıllık geliri  30k altında olan kısmın
#beklenen yaşam süresini boyuutu popülasyon, yılları da farklı renklere
#bölerek gösteren grafik.
