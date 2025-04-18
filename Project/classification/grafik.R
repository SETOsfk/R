library(tidyr)
library(ggplot2)
library(dplyr)

wine<-BordeauxWines
str(wine) #Öncelikle karakter tiplerini görmek için veri setini inceliyoruz
#İlk dört sütun harici bütün değerlerin faktör olması gerekirken double olarak işaretlenmiş
#Bunun dönüşümü için ilk dört sütun atılıyor.
wine<-wine[,-1:-4]
#Ardından double olan değişkenler faktöre dönüştürülüyor
wine<- wine %>% mutate_if(is.double,as.factor)
str(wine)
#Bazı faktörler sadece 0 olarak işaretlenmiş modeli eğitmede bir katkıda bulunmayacağını düşündüğüm için
#veri setinden çıkardım.
cols_to_remove <- sapply(wine, function(x) is.factor(x) && length(levels(x)) == 1)
wine_main<-wine[,!cols_to_remove]
#Hepsi faktör ve 0,1 olmak üzre iki levele sahip
str(wine_main)
#Çıkarılan sütunlar da veri setine geri ekleniyor ve son olarak YIL değişkeninin sınıf dönüşümü yapılıyor.
birlestir<-BordeauxWines[,1:4]
wine_main<-data.frame(birlestir,wine_main)
wine_main$Year<-as.Date(wine_main$Year)

#Skor değişkeni 90dan fazla olanlar ve 89dan az olanlar şeklinde ikiye ayrılıyor.
Diagnose <- c(1:14349)
wine_main<-data.frame(wine_main,Diagnose)
wine_main <- wine_main %>%
  mutate(Diagnose = case_when(
    Score <=89  ~0,
    Score >=90 ~1,)) %>% 
  select(Diagnose,Score,Name,Year,Price,everything())
wine_main$Diagnose<-as.factor(wine_main$Diagnose)
wine_main<- wine_main %>% select(-Year,-Name,-Price,-Score) #olduğunca sade bir subset elde etmek amacıyla
#skora etkisi olmayan değişkenler veri setinden atılıyor.



wine_0<-X_reduced %>% filter(Diagnose==0)
wine_1<-X_reduced %>% filter(Diagnose==1)
data_tbl_0 <- as_tibble(wine_0)
data_tbl_1 <- as_tibble(wine_1)
data_tbl<-as_tibble(wine_main)

filtered_wine_main_1.1 <- data_tbl_1 %>%
  select(where(~ sum(. == 1) >250))
filtered_wine_main_0.1 <- data_tbl_0 %>%
  select(where(~ sum(. == 1) >400))

filtered_wine_main_1.1<-X_reduced[,-1]
num_ones_tbl_1 <- X_reduced %>%
  summarise(across(everything(), ~ sum(. == 1))) %>%
  pivot_longer(cols = everything(), names_to = "Ozellik", values_to = "Sayi_1")

filtered_wine_main_0.1<-X_reduced[,-1]
num_ones_tbl_0 <- X_reduced %>%
  summarise(across(everything(), ~ sum(. == 1))) %>%
  pivot_longer(cols = everything(), names_to = "Ozellik", values_to = "Sayi_0")

# Sayıları birleştir

# Diagnose = 1 için sıklık grafiği
ggplot(num_ones_tbl_1, aes(y =reorder(Ozellik,Sayi_1), x =Sayi_1)) +
  geom_bar(stat = "identity", fill = "blue")+
  labs(title = "Diagnose = 1 (Skor> 90) icin Ozelliklerin Frekansi",
       x = "Frekans", y = "Ozellik")+
  theme_minimal()
# Diagnose = 0 için sıklık grafiği
ggplot(num_ones_tbl_0, aes(y = reorder(Ozellik, Sayi_0), x = Sayi_0)) +
  geom_bar(stat = "identity", fill = "red")+
  labs(title = "Diagnose = 0 (Skor < 90) icin Ozelliklerin Frekansi",
       x = "Frekans", y = "Ozellik") +
  theme_minimal()



