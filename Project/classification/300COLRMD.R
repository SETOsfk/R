library(dplyr)
library(ggplot2)
library(caret)
library(RANN)
library(tictoc)
library(readr)
BordeauxWines <- read_csv("~/R/İleri istatistiksel yazılımlar/final/final/BordeauxWines.csv")



#Veri setini oluşturduk
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
str(wine_main$Diagnose)

model_nb
model_gbm
model_lda
model_lda2
model_log
model_adaboost


#MODELLERİN KARŞILAŞTIRIILMASI.

fitted <- predict(model_nb, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_lda, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_tree, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")




models_compare.1 <- resamples(list(LOGREG= model_log, 
                                   LDA=model_lda,
                                   DECISIONTREE=model_tree,
                                   NAIVEBAYES=model_nb,
                                   GBM=model_gbm,
                                   ADABOOST=model_adaboost,
                                   SVM=svm_model
                                   
                                   
))




summary(models_compare.1)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare.1, scales=scales)





fitted <- predict(svm_model, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_lda, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_gbm, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_adaboost, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")















































