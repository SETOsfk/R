

#######PAKETLER#####
library(caret)
library(FactoMineR)
library(glmnet)
library(dplyr)
library(tidyr)
library(tictoc)
library(adabag)
library(xgboost)
library(kernlab)
###ÖN_İŞLEME#################

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
df<-wine_main
nzv <- nearZeroVar(df, saveMetrics = TRUE)
df <- df[, !nzv$nzv]


rm(wine)

#######TESTE_HAZIRLIK############
table(df$Diagnose)/length(df$Diagnose)
trainRowNumbers <- createDataPartition(df$Diagnose, p=0.70, list=FALSE) #şimdi iki marka karşılaştırıyoruz
#iki markanın da trainde anasette bulunduğu oranda bulunmasını sağlamak için
# Step 2: Egitim veri setinin olusturulmasi
trainData.1 <-df[trainRowNumbers,]
table(df$Diagnose)/length(df$Diagnose)
# Step 3: Test veri setinin olusturulmasi
testData.1 <-df[-trainRowNumbers,]
table(df$Diagnose)/length(df$Diagnose)
# X ve Y matrislerinin olusturulmasi
x = trainData.1[, -1]
y = trainData.1$Diagnose

#########MODEL_OLUŞTURMA##########
# Train naive bayes
set.seed(100)
tic()
model_nb.1 = train(Diagnose ~ ., data=trainData.1, method='naive_bayes')
model_nb.1
toc()
set.seed(100)
# Train lda
tic()
model_lda.1 = train(Diagnose ~ ., data=trainData.1, method='lda')
model_lda.1
toc()
set.seed(100)
# Train karar agaci
tictoc::tic()
model_tree.1 = train(Diagnose ~ ., data=trainData.1, method='rpart')
tictoc::toc()
model_tree.1

# resample() ile modellerin karsilastirilmasi
models_compare.1 <- resamples(list( LDA=model_lda.1, DECISIONTREE=model_tree.1, NAIVEBAYES=model_nb.1))

# Model performanslari
summary(models_compare.1)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare.1, scales=scales)

#Modellerin test performaslarinin karsilastirilmasi
set.seed(100)
fitted <- predict(model_nb.1, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")
set.seed(100)
fitted <- predict(model_lda.1, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")
set.seed(100)
fitted <- predict(model_tree.1, testData.1)

#CROSS VALIDATION YAPTIKTAN SONRA YENİ DEĞERLER, CROSS VALIDATION ÖNEMİ

fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 10  ) 

set.seed(100)
# Egitim knn
#tictoc::tic()
#model_knn.1 = train(Diagnose ~ ., data=trainData.1, method='knn',
#                  trControl=fitControl)
#tictoc::toc()
#model_knn.1

set.seed(100)
# Egitim naive bayes
tic()
model_nb.1 = train(Diagnose ~ ., data=trainData.1, method='naive_bayes',
                   trControl=fitControl)
toc()   
model_nb.1

set.seed(100)
# Egitim lda
tic()
model_lda.1 = train(Diagnose ~ ., data=trainData.1, method='lda',
                    trControl=fitControl)
toc() 
model_lda.1

set.seed(100)
# Egitim karar agaci
tic()
model_tree.1 = train(Diagnose ~ ., data=trainData.1, method='rpart',
                     trControl=fitControl)
toc()   
model_tree.1

# Model karsilastirma resample()
models_compare.1 <- resamples(list( LDA=model_lda.1, DECISIONTREE=model_tree.1, NAIVEBAYES=model_nb.1))

# Model performanslarinin karsilastirilmasi
summary(models_compare.1)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare.1, scales=scales)



set.seed(100)



set.seed(100)
# Egitim gbm
tic()
model_gbm.1 = train(Diagnose ~ ., data=trainData.1, method='gbm',
                    trControl = fitControl)
toc()
model_gbm.1

set.seed(100)





tic()
model_log.1<-train(Diagnose~.,data = trainData.1, method="logreg", 
                   trControl= fitControl)
toc()

tic()
model_adaboost <- train(Diagnose ~ ., data = trainData.1, method = "AdaBoost.M1", trControl =fitControl)
toc()





tic()
model_svm <- train(Diagnose ~ ., data = trainData.1, method = "svmRadial", trControl = fitControl)
toc()





# resample() ile modellerin karsilastirilmasi
models_compare.1 <- resamples(list(LOGREG= model_log.1, 
                                   LDA=model_lda.1,
                                   DECISIONTREE=model_tree.1,
                                   NAIVEBAYES=model_nb.1,
                                   GBM=model_gbm.1,
                                   ADABOOST=model_adaboost,
                                   SVM=model_svm))

# Model performanslarinin ozetlenmesi 
summary(models_compare.1)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare.1, scales=scales)





set.seed(100)
fitted <- predict(model_adaboost, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")
set.seed(100)
fitted <- predict(model_svm, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")
set.seed(100)
fitted <- predict(model_gbm.1, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")














