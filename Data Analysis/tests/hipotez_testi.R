vize<-c(25,41,41,54,29,50,
        54,46,54,33,33,54,
        37,12,29,41)
#A��klanan vize notlar� �zerinden dersi veren ��retim �yesi
#dersin ortalamas�n�n 60 a e�it olup olmad���n� test etmek istiyor. 
#Ha:mu=60
#Standart sapma (sigma) biliniyor mu?
#t testi 
#1. Normallik varsay�m�
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
#2. p-value<.01 oldu�undan o reddedilir.
#3. G�ven aral���=[32.99, 46.13] 60 i�ermedi�inden Ho hip reddedilir.

t.test(x=vize, 
       alternative = "two.sided", 
       conf.level = 0.95,
       mu=35)
#1. thesap=1.48
qt(0.025, df=15) #tkritik
#thesap<tkritik oldugundan Ho reddilemez.
#2. p-value=0.15>0.05 oldu�undan Ho reddedilemez.
#3. G�ven aral���=[32.99, 46.13] 35 i�erdi�inden Ho hip reddilemez.

t.test(x=vize, 
       alternative = "greater", 
       conf.level = 0.95,
       mu=35)
#1. thesap=1.48
qt((1-0.05), df=15) #tkritik
#thesap<tkritik oldugundan Ho reddilemez.
#2. p-value=0.08>0.05 oldu�undan Ho reddedilemez.
#3. G�ven aral���=[34.16, INF] 35 i�erdi�inden Ho hip reddilemez.

library(BSDA)
?z.test
#Bu ��renci grubu i�in sigma=10 puan olarak bilinmektedir.
#H0:mu=60
z.test(x=vize, alternative = "two.sided",
       conf.level = 0.95, mu=60,
       sigma.x=10)
qnorm(0.025)
#1. tkritik=1.96<thesap=8.17 Ho reddedilir.
#2. pvalue<.01 Ho reddedilir
#3. G�ven aral���=[34.66, 44.46] 60 de�erini i�ermiyor. 
#Ho reddedilir.

#Ayn� vize s�nav� A ve B gruplar�nda yap�l�yor. 
Vize.A<-c(25,41,41,54,29,50,
          54,46,54,33,33,54,
          37,12,29,41)
Vize.B<-c(41,66,92,71,71,54,
          88,54,70,50,58,79,
          88,46,67,46)
#Ho:mu1=mu2
#Ho:mu1-mu2=0
#Ho:mu1-mu2=D
#Ba��ml�l�k? �ki grup tamamen farkl� ��rencilerden olu�tu�u i�in ba��ms�zd�r
#Normallik
shapiro.test(Vize.A)
shapiro.test(Vize.B)
#Varyans Homojenli�i
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
#Guven aral�g�=[-35,-15] 0 i�ermed�ine g�re fark vard�r. A grubu B grubuna 
#g�re hep daha d���k ortalama �retmektedir.

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
#Guven aral�g�=[-INF,-16] 0 i�ermed�ine g�re fark vard�r. A grubu B grubuna 
#g�re hep daha d���k ortalama �retmektedir.

#Ayn� vize s�nav� A ve B gruplar�nda yap�l�yor. 
Vize.�ncesi<-c(25,41,41,54,29,50,
          54,46,54,33,33,54,
          37,12,29,41)
Vize.sonrasi<-c(41,66,92,71,71,54,
          88,54,70,50,58,79,
          88,46,67,46)

#Normallik 
fark<-Vize.sonrasi-Vize.�ncesi
shapiro.test(fark)
#Ba��ms�zl�k--��renciler de�i�medi�i i�in gruplar ba��ml�d�r.

#Ho: mu.sonrasi-mu.�ncesi=D
t.test(fark, mu=0, alternative = "two.sided")
#thesap=6.78>tkritik Buna g�re Ho reddedilir.
tkritik<-qt(0.025, 15)
#p-value<.01 old Ho reddedilir.
#G�ven aral���=[17.5,33.5] 0 i�ermedi�inden Ho reddedilir.

#Ho: mu.sonrasi-mu.�ncesi=20
t.test(fark, mu=20, alternative = "two.sided")
#thesap=1.46>tkritik Buna g�re Ho reddedilemez.
tkritik<-qt(0.025, 15)
#p-value=.16>.05 old Ho reddedilemez.
#G�ven aral���=[17.5,33.5] 20 i�ermedi�inden Ho reddedilebilir. 
#Bu iki zaman aras�nda uygulanan y�ntem ortalama 20 puanl�k art��
#sa�lam��t�r.