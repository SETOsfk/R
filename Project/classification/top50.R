####Paket####
library(dplyr)
library(ggplot2)
library(caret)
library(RANN)
library(tictoc)
library(tidyr)
library(adabag)

library(xgboost)
library(kernlab)
#####
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


wine_0<- wine_main %>% filter(Diagnose==0)#0 ve 
wine_1<- wine_main %>% filter(Diagnose==1)#1 ler ayrı veri setine alınıyor



# Convert the data frame to a tibble for easier manipulation with dplyr
#dplyr ile daha kolay bir şekilde manipüle edilmesi için veri seti tibblea dönüştürüldü.
data_tbl_0 <- as_tibble(wine_0)
data_tbl_1 <- as_tibble(wine_1)
data_tbl<-as_tibble(wine_main)

num_ones_tbl <- data_tbl %>%
  summarise(across(everything(), ~ sum(. == 1))) #0 ve 1ler iiçeren veri setinde
#her sütun için kaç defa birlerin gözüktüğünü hesaplatan kod

# Print the result
num_ones_tbl<-t(num_ones_tbl)

# Calculate the number of 1s in each column
num_ones_tbl_1 <- data_tbl_1 %>%
  summarise(across(everything(), ~ sum(. == 1)))
#90+ skora sahip verilerde her sütun için kaç defa bir kullanıldığını gösteriyor
#ne kadar fazla görülürse o kadar iyi böylelikle bir şarabın 90+
#skor almasını sağlayan kelimeleri çok daha net bir şekilde görebiliriz.

num_ones_tbl_1<-t(num_ones_tbl_1)

num_ones_tbl_0 <- data_tbl_0 %>%
  summarise(across(everything(), ~ sum(. == 1))) #aynısı sıfır için de yapılıyor

# Print the result
num_ones_tbl_0<-t(num_ones_tbl_0)


# Filter columns
filtered_wine_main_1.1 <- data_tbl_1 %>%
  select(where(~ sum(. == 1) > 284))
filtered_wine_main_1.1<-as.data.frame(filtered_wine_main_1.1) #1ler için ilk elli sütun seçiliyor.
filtered_wine_main_0.1 <- data_tbl_0 %>%
  select(Diagnose,where(~ sum(. == 1) > 490))
filtered_wine_main_0.1<-as.data.frame(filtered_wine_main_0.1)
# Combine data frames by rows and remove duplicates

# Combine data frames by rows and remove duplicates
isim.0<-colnames(filtered_wine_main_0.1)
isim.1<-colnames(filtered_wine_main_1.1)
filtered_wine_main.1<-data_tbl %>% select(all_of(isim.0),all_of(isim.1))

# Assuming data1 and data2 are your datasets
# Find rows in data1 but not in data2

table(filtered_wine_main.1$Diagnose)/length(filtered_wine_main.1$Diagnose)
trainRowNumbers <- createDataPartition(filtered_wine_main.1$Diagnose, p=0.70, list=FALSE) #şimdi iki marka karşılaştırıyoruz
#iki markanın da trainde anasette bulunduğu oranda bulunmasını sağlamak için
# Step 2: Egitim veri setinin olusturulmasi
trainData.1 <- filtered_wine_main.1[trainRowNumbers,]
table(trainData.1$Diagnose)/length(trainData.1$Diagnose)
# Step 3: Test veri setinin olusturulmasi
testData.1 <- filtered_wine_main.1[-trainRowNumbers,]
table(testData.1$Diagnose)/length(testData.1$Diagnose)
# X ve Y matrislerinin olusturulmasi




# Train naive bayes
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
fitted <- predict(model_nb.1, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_lda.1, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_tree.1, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

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



library(randomForest)
set.seed(100)



#Ensemble yontemlerin egitilmesi
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
model_log.1
toc()
tic()
model_adaboost <- train(Diagnose ~ ., data = trainData.1, method = "AdaBoost.M1", trControl =fitControl)
toc()





tic()
svm_model <- train(Diagnose ~ ., data = trainData.1, method = "svmRadial", trControl = fitControl)
toc()


# resample() ile modellerin karsilastirilmasi
models_compare.1 <- resamples(list(LOGREG= model_log.1, 
                                 LDA=model_lda.1,
                                DECISION_TREE=model_tree.1,
                                 NAIVEBAYES=model_nb.1,
                                 GBM=model_gbm.1,
                                 ADABOOST=model_adaboost,
                                 SVM=svm_model
                              
                                
                                 ))

# Model performanslarinin ozetlenmesi 
summary(models_compare.1)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare.1, scales=scales)


fitted <- predict(svm_model, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_gbm.1, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_adaboost, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")

fitted <- predict(model_lda.1, testData.1)
confusionMatrix(reference = testData.1$Diagnose, data = as.factor(fitted), mode='everything', positive="1")



svm_model$


