
######PAKETLER#######

library(dplyr)
library(ggplot2)
library(caret)
library(RANN)
library(tictoc)
library(tidyr)
library(adabag)
library(readr)
library(xgboost)
library(kernlab)
BordeauxWines <- read_csv("~/R/İleri istatistiksel yazılımlar/final/final/BordeauxWines.csv")
###ÖN_İŞLEME############
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
    Score <=89  ~"X0",
    Score >=90 ~"X1",)) %>% 
  select(Diagnose,Score,Name,Year,Price,everything())
wine_main$Diagnose<-as.factor(wine_main$Diagnose)
wine_main<- wine_main %>% select(-Year,-Name,-Price,-Score) #olduğunca sade bir subset elde etmek amacıyla
#skora etkisi olmayan değişkenler veri setinden atılıyor.
str(wine_main$Diagnose)


data_tbl<-as_tibble(wine_main)

filtered_wine_main <- data_tbl %>%
  select(where(~ sum(. == 1) > 35))
filtered_wine_main<-as.data.frame(filtered_wine_main)#genel veri seti için ilk elli sütun seçiliyor.

table(filtered_wine_main$Diagnose)/length(filtered_wine_main$Diagnose)
set.seed(256423)
trainRowNumbers <- createDataPartition(wine_main$Diagnose, p=0.70, list=FALSE) #şimdi iki marka karşılaştırıyoruz
#iki markanın da trainde anasette bulunduğu oranda bulunmasını sağlamak için
# Step 2: Egitim veri setinin olusturulmasi

trainData <- filtered_wine_main[trainRowNumbers,]
table(trainData$Diagnose)/length(trainData$Diagnose)
# Step 3: Test veri setinin olusturulmasi
testData <- filtered_wine_main[-trainRowNumbers,]
table(testData$Diagnose)/length(testData$Diagnose)
# X ve Y matrislerinin olusturulmasi
rm(Diagnose)

###########MODEL_OLUŞTURMA###########

# Train naive bayes
model_nb = train(Diagnose ~ ., data=trainData, method='naive_bayes')
model_nb

set.seed(100)
# Train lda
model_lda = train(Diagnose ~ ., data=trainData, method='lda')
model_lda

set.seed(100)
# Train karar agaci
tictoc::tic()
model_tree = train(Diagnose ~ ., data=trainData, method='rpart')
tictoc::toc()
model_tree

# resample() ile modellerin karsilastirilmasi
models_compare <- resamples(list(LDA=model_lda, DECISIONTREE=model_tree, NAIVEBAYES=model_nb))

# Model performanslari
summary(models_compare)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)

#Modellerin test performaslarinin karsilastirilmasi
fitted <- predict(model_nb, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_lda, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_tree, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")
 fitControl <- trainControl(
                  method = 'cv',                   # k-fold cross validation
                  number = 10  ) 

                
                set.seed(100)
                # Egitim naive bayes
                model_nb = train(Diagnose ~ ., data=trainData, method='naive_bayes',
                                 trControl=fitControl)
                model_nb
                
                set.seed(100)
                # Egitim lda
                model_lda = train(Diagnose ~ ., data=trainData, method='lda',
                                  trControl=fitControl)
                model_lda
                
                set.seed(100)
                # Egitim karar agaci
                model_tree = train(Diagnose ~ ., data=trainData, method='rpart',
                                   trControl=fitControl)
                model_tree
                
                # Model karsilastirma resample()
                models_compare <- resamples(list(LDA=model_lda, DECISIONTREE=model_tree, NAIVEBAYES=model_nb))
                
                # Model performanslarinin karsilastirilmasi
                summary(models_compare)
                scales <- list(x=list(relation="free"), y=list(relation="free"))
                bwplot(models_compare, scales=scales)
                

#Ensemble yontemlerin egitilmesi
set.seed(100)
# Egitim gbm
model_gbm = train(Diagnose ~ ., data=trainData, method='gbm',
                  trControl = fitControl)
model_gbm

set.seed(100)
# Egitim random forest




tic()
model_log<-train(Diagnose~.,data = trainData, method="logreg", 
                 trControl= fitControl)
toc()
model_log
tic()
model_adaboost <- train(Diagnose ~ ., data = trainData, method = "AdaBoost.M1", trControl =fitControl)
toc()

model_adaboost



tic()
svm_model <- train(Diagnose ~ ., data = trainData, method = "svmRadial", trControl = fitControl)
toc()
svm_model

# resample() ile modellerin karsilastirilmasi
models_compare.1 <- resamples(list(LOGREG= model_log, 
                                   LDA=model_lda,
                                   DECISIONTREE=model_tree,
                                   NAIVEBAYES=model_nb,
                                   GBM=model_gbm,
                                   ADABOOST=model_adaboost,
                                   SVM=svm_model,
                                   NAIVE_BAYES=model_nb
                                   
                                   
))

# Model performanslarinin ozetlenmesi 
summary(models_compare.1)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare.1, scales=scales)


# Model performanslarinin ozetlenmesi 

#MODASDASDIHA
###SALKDJSALKDAJSKŞLDJAŞ


fitted <- predict(svm_model, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_lda, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_gbm, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_adaboost, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="1")


