vize<-c(25,41,41,54,29,50,
        54,46,54,33,33,54,
        37,12,29,41)
#Açýklanan vize notlarý üzerinden dersi veren öðretim üyesi
#dersin ortalamasýnýn 60 a eþit olup olmadýðýný test etmek istiyor. 
#Ha:mu=60
#Standart sapma (sigma) biliniyor mu?
#t testi 
#1. Normallik varsayýmý
hist(vize)
boxplot(vize)
qqnorm(vize)
qqline(vize)
shapiro.test(vize)

?t.test
t.test(x=vize, 
       alternative = "two.sided", 
       conf.level = 0.95,
       mu=60)
#1. thesap=-6.63 
qt(0.025, df=15) #tkritik
#thesap>tkritik oldugundan Ho reddedilir.
#2. p-value<.01 olduðundan o reddedilir.
#3. Güven aralýðý=[32.99, 46.13] 60 içermediðinden Ho hip reddedilir.

t.test(x=vize, 
       alternative = "two.sided", 
       conf.level = 0.95,
       mu=35)
#1. thesap=1.48
qt(0.025, df=15) #tkritik
#thesap<tkritik oldugundan Ho reddilemez.
#2. p-value=0.15>0.05 olduðundan Ho reddedilemez.
#3. Güven aralýðý=[32.99, 46.13] 35 içerdiðinden Ho hip reddilemez.

t.test(x=vize, 
       alternative = "greater", 
       conf.level = 0.95,
       mu=35)
#1. thesap=1.48
qt((1-0.05), df=15) #tkritik
#thesap<tkritik oldugundan Ho reddilemez.
#2. p-value=0.08>0.05 olduðundan Ho reddedilemez.
#3. Güven aralýðý=[34.16, INF] 35 içerdiðinden Ho hip reddilemez.

library(BSDA)
?z.test
#Bu öðrenci grubu için sigma=10 puan olarak bilinmektedir.
#H0:mu=60
z.test(x=vize, alternative = "two.sided",
       conf.level = 0.95, mu=60,
       sigma.x=10)
qnorm(0.025)
#1. tkritik=1.96<thesap=8.17 Ho reddedilir.
#2. pvalue<.01 Ho reddedilir
#3. Güven aralýðý=[34.66, 44.46] 60 deðerini içermiyor. 
#Ho reddedilir.

#Ayný vize sýnavý A ve B gruplarýnda yapýlýyor. 
Vize.A<-c(25,41,41,54,29,50,
          54,46,54,33,33,54,
          37,12,29,41)
Vize.B<-c(41,66,92,71,71,54,
          88,54,70,50,58,79,
          88,46,67,46)
#Ho:mu1=mu2
#Ho:mu1-mu2=0
#Ho:mu1-mu2=D
#Baðýmlýlýk? Ýki grup tamamen farklý öðrencilerden oluþtuðu için baðýmsýzdýr
#Normallik
shapiro.test(Vize.A)
shapiro.test(Vize.B)
#Varyans Homojenliði
var.test(x=Vize.A, y=Vize.B)
#p-value=0.31>0.05 old Ho reddedilemez.

t.test(x=Vize.A, y=Vize.B, 
       alternative = "two.sided",
       conf.level = 0.95,
       var.equal = TRUE,
       paired = FALSE,
       mu=0)
#1. thesap=-5.02
tkritik<-qt(0.025, df=30)
#thesap>tkritik old. Ho reddedilir
#p-value<.01 old Ho reddedilir
#Guven aralýgý=[-35,-15] 0 içermedðine göre fark vardýr. A grubu B grubuna 
#göre hep daha düþük ortalama üretmektedir.

t.test(x=Vize.A, y=Vize.B, 
       alternative = "less",
       conf.level = 0.95,
       var.equal = TRUE,
       paired = FALSE,
       mu=0)
#1. thesap=-5.02
tkritik<-qt(0.05, df=30)
#thesap>tkritik old. Ho reddedilir
#p-value<.01 old Ho reddedilir
#Guven aralýgý=[-INF,-16] 0 içermedðine göre fark vardýr. A grubu B grubuna 
#göre hep daha düþük ortalama üretmektedir.

#Ayný vize sýnavý A ve B gruplarýnda yapýlýyor. 
Vize.öncesi<-c(25,41,41,54,29,50,
          54,46,54,33,33,54,
          37,12,29,41)
Vize.sonrasi<-c(41,66,92,71,71,54,
          88,54,70,50,58,79,
          88,46,67,46)

#Normallik 
fark<-Vize.sonrasi-Vize.öncesi
shapiro.test(fark)
#Baðýmsýzlýk--öðrenciler deðiþmediði için gruplar baðýmlýdýr.

#Ho: mu.sonrasi-mu.öncesi=D
t.test(fark, mu=0, alternative = "two.sided")
#thesap=6.78>tkritik Buna göre Ho reddedilir.
tkritik<-qt(0.025, 15)
#p-value<.01 old Ho reddedilir.
#Güven aralýðý=[17.5,33.5] 0 içermediðinden Ho reddedilir.

#Ho: mu.sonrasi-mu.öncesi=20
t.test(fark, mu=20, alternative = "two.sided")
#thesap=1.46>tkritik Buna göre Ho reddedilemez.
tkritik<-qt(0.025, 15)
#p-value=.16>.05 old Ho reddedilemez.
#Güven aralýðý=[17.5,33.5] 20 içermediðinden Ho reddedilebilir. 
#Bu iki zaman arasýnda uygulanan yöntem ortalama 20 puanlýk artýþ
#saðlamýþtýr.