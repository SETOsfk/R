library(caret)
library(RANN)
library(tictoc)


# Step 1: Egitim ve test verisinin olusturulmasi
table(wine_main$Grouped_Score)/length(wine_main$Grouped_Score)
trainRowNumbers <- createDataPartition(wine_main$Grouped_Score, p=0.75, list=FALSE) #şimdi iki marka karşılaştırıyoruz
#iki markanın da trainde anasette bulunduğu oranda bulunmasını sağlamak için
# Step 2: Egitim veri setinin olusturulmasi
trainData <- wine_main[trainRowNumbers,]
table(trainData$Grouped_Score)/length(trainData$Grouped_Score)
# Step 3: Test veri setinin olusturulmasi
testData <- wine_main[-trainRowNumbers,]
table(testData$Grouped_Score)/length(testData$Grouped_Score)
# X ve Y matrislerinin olusturulmasi
x = trainData[, 2:621]
y = trainData$Score




#Model gelistirme
modelnames <- paste(names(getModelInfo()), collapse=', ')
modelnames
#knn lazy learner çünkü sadece komşulara bakıyor hiç hesap yapmıyor.
set.seed(100)
# knn modelinin egitilmesi
tictoc::tic()
model_knn = train(Score ~ ., data=trainData, method='knn')
tictoc::toc()
model_knn
model_knn$results

plot(model_knn, main="Model Dogruluk Orani")
#NO INFIRMATION RATE:
#DOĞRULUK ORANININ ONDAN YÜKSEK OLMASI LAZIM
#YANİ %61 %39 ORANLA CH MM ALIYODU ZATEN
#RASTGELE ATSAN DA BUNLARI TUTTURURSUN
#BİZİM AMACIMIZ, BU DEĞERLERİN ÜZERİNE ÇIKMAK.

#Test performansinin incelenmesi
testData2 <- predict(preProcess_missingdata_model, testData)
testData3 <- predict(dummies_model, testData2)
testData4 <- predict(preProcess_range_model, testData3)
head(testData4[, 1:10])
fitted <- predict(model_knn, testData4)
confusionMatrix(reference = as.factor(testData$Purchase), 
                data = as.factor(fitted), mode='everything', 
                positive='MM')


#Diger modellerin egitimi
set.seed(100)

# Train naive bayes
model_nb = train(Purchase ~ ., data=trainData, method='naive_bayes')
model_nb

set.seed(100)
# Train lda
model_lda = train(Purchase ~ ., data=trainData, method='lda')
model_lda

set.seed(100)
# Train karar agaci
tictoc::tic()
model_tree = train(Purchase ~ ., data=trainData, method='rpart')
tictoc::toc()
model_tree

# resample() ile modellerin karsilastirilmasi
models_compare <- resamples(list(KNN=model_knn, LDA=model_lda, DECISIONTREE=model_tree, NAIVEBAYES=model_nb))

# Model performanslari
summary(models_compare)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)

#Modellerin test performaslarinin karsilastirilmasi
fitted <- predict(model_nb, testData4)
confusionMatrix(reference = as.factor(testData$Purchase), data = fitted, mode='everything', positive='MM')

fitted <- predict(model_lda, testData4)
confusionMatrix(reference = as.factor(testData$Purchase), data = fitted, mode='everything', positive='MM')

fitted <- predict(model_tree, testData4)
confusionMatrix(reference = as.factor(testData$Purchase), data = fitted, mode='everything', positive='MM')

#Degisken onem duzeyleri
varimp_knn <- varImp(model_knn)
plot(varimp_knn, main="knn icin degisken onem duzeyleri")
varimp_knn <- varImp(model_tree)
plot(varimp_knn, main="knn icin degisken onem duzeyleri")

#trainControl() fonksiyonunu kullanmak
?trainControl
fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 10                      # fold sayisi
) 

set.seed(100)
# Egitim knn
fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 10                      # fold sayisi
) 

tictoc::tic()
model_knn = train(Purchase ~ ., data=trainData, method='knn',
                  trControl=fitControl)
tictoc::toc()
model_knn

set.seed(100)
# Egitim naive bayes
model_nb = train(Purchase ~ ., data=trainData, method='naive_bayes',
                 trControl=fitControl)
model_nb

set.seed(100)
# Egitim lda
model_lda = train(Purchase ~ ., data=trainData, method='lda',
                  trControl=fitControl)
model_lda

set.seed(100)
# Egitim karar agaci
model_tree = train(Purchase ~ ., data=trainData, method='rpart',
                   trControl=fitControl)
model_tree

# Model karsilastirma resample()
models_compare <- resamples(list(KNN=model_knn, LDA=model_lda, DECISIONTREE=model_tree, NAIVEBAYES=model_nb))

# Model performanslarinin karsilastirilmasi
summary(models_compare)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)

#Recursive Feature Elimination (RFE) yontemi ile ozellik secimi
set.seed(100)

subsets <- c(1:5, 10, 15, 18)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=trainData[,1:18], y=as.factor(trainData$Purchase),
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

#Ensemble yontemlerin egitilmesi
set.seed(100)
# Egitim gbm
model_gbm = train(Purchase ~ ., data=trainData, method='gbm',
                  trControl = fitControl)
model_gbm

set.seed(100)
# Egitim random forest
model_rf = train(Purchase ~ ., data=trainData, method='rf', 
                 trControl = fitControl)
model_rf

# resample() ile modellerin karsilastirilmasi
models_compare <- resamples(list(KNN=model_knn, 
                                 LDA=model_lda,
                                 DECISIONTREE=model_tree,
                                 NAIVEBAYES=model_nb,
                                 GBM=model_gbm,
                                 RANDOMFOREST=model_rf))

# Model performanslarinin ozetlenmesi 
summary(models_compare)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)

#Final MODEL
model_gbm2 = train(Purchase ~ LoyalCH+PriceDiff+StoreID, data=trainData, method='gbm',
                   trControl = fitControl)
model_gbm2