
library(tidyverse)
library(patchwork)
library(forcats)
head(iris)

#Chi squared goodnes of fit:
#Small medium large arasında fark var mı? 

#Chi squared test of independence
#small medium largeın içerdiği veriler bağımlılığı mı etkiliyor
#yani smalldaki bir verinin değerini bilmek bize mediumdaki bir değeri söyler mi
#Bağımlı mı?



#Numeric verileri kategoriğe recode ediyoruz
flowers=iris%>%
  mutate(Size=cut(Sepal.Length,#sepal lenghti KES
                  breaks=3,#3 eşit parça olan
                  labels= c("Small","Medium","Large")))%>% #küçük,orta büyüğe.
           select(Species,Size)#species ve size ı seç
table(flowers)#tablo haline getir.


#Question: Small,medium ve large arasında
#bir fark var mıdır
#Chi squared goodness of fit
#H0: Bütün çiçeklerin boyu aynıdır
#H1: Değildir
chisq.test(table(flowers$Size))
#aynısının pipelı hali okuması daha kolay. 
flowers%>%
  select(Size)%>%
  table()%>%
  chisq.test()
#H0 red, H1 Kabul means: Evet aralarında bir fark vardır. eşit değiller
#p value= 0.0000006673 alpha değerinden baya küçük.

#Chi squared test of independence
#(bağımsızlık testi)

#H0: Değerler bağımsız

#Değerler arasında bir ilişki yok
#Birinin diğerini bilmek diğerini tahmin etmeye etkimez.

#H1: Değerler bağımlı.
#Birinin değerini bilmek
#Diğerini bilmeye etkir.

table(flowers)

flowers%>%
  table()%>%#Size diye belirtmedik türleri de alıcak ki bağımlılığını kontrol edebilsin.
  chisq.test()
#Null hipotezi red, bu değerler bağımlı birini bilmek diğerine etkir.


#fishers exact test if >20% of expected
#values are <5 or all are if any 
#values of <5 in 2x2

flowers%>%
  table()%>%
  chisq.test()%>%
  .$expected
#Hiçbiri 5ten küçük değil fisherin kesin olasılık testini
#kullanmak zorunda değiliz.
my_test=flowers%>%
  table()%>%
  chisq.test()
my_test
attributes(my_test)
my_test$expected





View(gss_cat)

my_data=gss_cat%>%
  select(marital)%>%
  filter(marital %in% c("Married",
                        "Never married",
                        "Divorced"))%>%
  mutate(marital=fct_drop(marital))
#Kıscasaı bunları topladık
my_table=table(my_data)
View(my_table)
chisq.test(my_table)
my_data%>%
  table()%>%
  chisq.test()
#H0 Aralarında fark yok
#H1 aralarında fark vardı 
#Evlenen,evlenmeyen,boşanan insanlar arasında fark var