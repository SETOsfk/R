library(tidyverse)
library(gapminder)
#Burda analiz yapmıyorsun elindeki veri setini anlamaya çalışıyorsun, nereye bakman gerektiği
#Neyin önemli olduğu, hatalı giriş var mı yok mu, kayıp verilerin neler, nerdeler.
#Verinin içine giriyorsun.
?starwars
View(starwars)
dim(starwars)#x row y variables bilgisini verir
str(starwars)#Bu biraz karmaşık duruyor ama kısa bir özet verir.
glimpse(starwars)#Glimpse yukardaki ikisinin birleşimi gibi çok daha iyi.
#Buraya baktığında kategorik veri göremiyorsun R genelde kategorik verileri
#Character olarak alıyor buna dikkat et.
head(starwars)
tail(starwars)#Bunlar da arada işine yarayabilir.
starwars$name#Name i gösterir sana

attach(starwars)#ama attach dersen dolar işareti koymana gerek kalmaz vb çok da önemli değil

names(starwars)#Bu da işine yarayabilir kolon seçerken vb
length(starwars)


class(hair_color)  
unique(hair_color)#all of the unique values in that variable
View(sort(table(hair_color),decreasing = TRUE))
barplot(sort(table(hair_color),decreasing = TRUE))
barplot(sort(table(eye_color),decreasing = TRUE))
table(hair_color)

starwars%>%
select(hair_color)%>%count(hair_color)%>% arrange(desc(n))%>%View

#Eğer kayıp veri sistematikse yani belli  bir düzene, örüntüye, vb sahipse
#Bu baya önemli olabilir bunnu göz önünde bulundurmak lazım.

View(starwars[is.na(hair_color),])#hair colordaki NA leri göster
               #eğer saç       Column boş bıraktık ki bütün verileri kullansın. belli bir aralığa bakmasın
#                NA İSE
                #bu rowu seç

starwars#[#seçmek istedin row#,#seçmek istedin column#]
  is.na(hair_color)

View(starwars[is.na(hair_color),])#Saçı NA olanları göster
#Sonrasında kolonlara bak saç rengi NA olanlar aynı yılda mı doğumuş aynı yerde mi doğmuş
#Skin coloru aynımıymış vb
#Baktığında görüyorsun ki bunların 3'ü droidmiş 

summary(height)
hist(height)
boxplot(height)
#Eğer numericlerle ğraşıyorsan

