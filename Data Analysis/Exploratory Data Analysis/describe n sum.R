#Describe n summarise

#Numeric verilerle çalışıcaz birkaç şeyini describe edicez
#Range/spread: Belli bir aralık belirle ya da dağıt
#Centrality:
#Variance
#Summarise your data
#Create tables


library(tidyverse)
View(msleep)

#Have a quick look
glimpse(msleep)
#Describe the spread, centrality and variance
#selected variables
min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)
mean(msleep$awake)
median(msleep$awake)
var(msleep$awake) #teker teker yazabilirsin ya da

summary(msleep)#Bu da işine yarayabilir
summary(msleep$sleep_total)#Belli bir kolonu seçebilirsin
msleep%>% select(sleep_total,brainwt)%>% summary#Belli kolonları seçebilirsin.

#Create a summary table
#for each category VORE
#show min max diff
#and average sleep_total
#and arrange data by the average
msleep%>% drop_na(vore)%>%
  group_by(vore)%>%
  summarise(Lower=min(sleep_total),Upper=max(sleep_total),
            average=mean(sleep_total),
            diff=max(sleep_total)-min(sleep_total))%>%
  arrange(average)
#Creating contingency tables
library(MASS)
attach(Cars93)
#Attach dedik cars93$ yapmamıza gerek yok.
glimpse(Cars93)
table(Origin) #Arabaların originini tablo halinde göster

table(AirBags,Origin) #Airbags ve origini gösteren bir birleşik tablo oluştur
#airbagi olan arabaların kaçı us,non-us originli vb.

addmargins(table(AirBags,Origin),1)
#Toplamlarını aşağıdaki bir satıra ekler eğer 1 yerine 2 koysaydık bir sütüna ekleyecekti.
#Eğer hiçbir şey koymasaydık oraya hem satıra hem sütuna ekleyecekti

#OLASILIK TABLSOU
addmargins(prop.table(table(AirBags,Origin))*100)#Bütün tablo add up to %100
addmargins(prop.table(table(AirBags,Origin),1)*100)
#Sadece satırlar add up to %100
#YANİ veri setindeki airbagi none olanları ele alırsak. 
#AİRBAGİ NONE OLANLARIN %47Sİ USA DE %52Sİ USADE DEĞİL şeklinde okunabilir.
#Numalara yerine bize olasılıklar veriyor 100 ile çarptık direkt % olarak okuyabiliriz.

round(addmargins(prop.table(table(AirBags,Origin),1)*100))
#Direkt yuvarladık okuması daha kolay.
round(table(AirBags,Origin)) #yuvarlayarak yaz.

#BASE R KULLANARAK OLUŞTURDUĞUMUZ TABLOYU
#TİDYVERSE KULLANARAK DA YAPALIM


Cars93%>%
  group_by(Origin,AirBags)%>%#Origin ve airbage göre grupla
  summarise(number=n())%>%#number adında yeni kolon oluştur, ve saylarını göster n()
  pivot_wider(names_from=Origin, #nameleri origin kolonundan
              values_from = number)#sayıları da number kolonundan al
 