

# Load necessary packages
library(caret)
library(FactoMineR)
library(glmnet)
library(dplyr)
library(tidyr)
library(tictoc)
library(adabag)
library(xgboost)
library(kernlab)
library(randomForest)
library(ROSE)
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

rm(birlestir)
rm(wine)
rm(cols_to_remove)
rm(Diagnose)


# Create a synthetic dataset for demonstration


# Train a random forest model
rf_model <- randomForest(Diagnose ~ ., data = wine_main, importance = TRUE, ntree = 100)

# Print the model summary
print(rf_model)

# Extract feature importances
importance_scores <- importance(rf_model, type = 1)

# Print the importance scores
head(importance_scores)


# Assuming rf_model is already trained as shown in the previous example
# Extract feature importances


# Check the structure of the importance_scores
str(importance_scores)

# If importance_scores is a matrix, extract the correct column
# Typically the first column contains MeanDecreaseAccuracy or MeanDecreaseGini
if (is.matrix(importance_scores)) {
  importance_values <- importance_scores[, 1] # Change 1 if another column is needed
} else {
  importance_values <- importance_scores
}

# Sort the importance scores in decreasing order and select the top 50 features
sorted_importance <- sort(importance_values, decreasing = TRUE)
important_features <- names(sorted_importance)[1:50]


# Select the top 50 important features



print(important_features)

X_reduced <- wine_main[,c(important_features)]
Diagnose<-wine_main$Diagnose
X_reduced<- data.frame(Diagnose,X_reduced) 

str(X_reduced)

barplot(prop.table(table(X_reduced$Diagnose)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Sınıf Dağılımı")





set.seed(200)
trainRowNumbers <- createDataPartition(X_reduced$Diagnose, p=0.70, list=FALSE) #şimdi iki marka karşılaştırıyoruz
#iki markanın da trainde anasette bulunduğu oranda bulunmasını sağlamak için
# Step 2: Egitim veri setinin olusturulmasi

trainData <-X_reduced[trainRowNumbers,]
table(trainData$Diagnose)/length(trainData$Diagnose)
# Step 3: Test veri setinin olusturulmasi
testData <- X_reduced[-trainRowNumbers,]
table(testData$Diagnose)/length(testData$Diagnose)

set.seed(200)
bothq <- ovun.sample(Diagnose~., data = trainData, method = "both")
trainData<-bothq$data

barplot(prop.table(table(trainData$Diagnose)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Sınıf Dağılımı")




tic()
set.seed(100)
model_nb = train(Diagnose ~ ., data=trainData, method='naive_bayes')
model_nb
toc()
set.seed(100)
# Train lda
tic()
model_lda = train(Diagnose ~ ., data=trainData, method='lda')
model_lda
toc()
set.seed(100)
# Train karar agaci
tic()
set.seed(100)
model_tree = train(Diagnose ~ ., data=trainData, method='rpart')
toc()
model_tree

# resample() ile modellerin karsilastirilmasi
models_compare <- resamples(list(LDA=model_lda, DECISIONTREE=model_tree, NAIVEBAYES=model_nb))

# Model performanslari
summary(models_compare)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)


fitted <- predict(model_nb, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="X1")

fitted <- predict(model_lda, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="X1")

fitted <- predict(model_tree, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="X1")




fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 10) 


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
set.seed(100)
model_log<-train(Diagnose~.,data = trainData, method="glm", 
                 trControl= fitControl)
toc()
model_log
tic()
set.seed(100)
model_adaboost <- train(Diagnose ~ ., data = trainData, method = "AdaBoost.M1", trControl =fitControl)
toc()

model_adaboost



tic()
set.seed(100)
svm_model <- train(Diagnose ~ ., data = trainData, method = "svmRadial", trControl = fitControl)
toc()
svm_model


# Define the control for caret

# Train the XGBoost model using caret
train_control <- trainControl(
  method = "cv", 
  number = 10, 
  verboseIter = TRUE,
  classProbs = TRUE,               # Ensure class probabilities are computed
  summaryFunction = twoClassSummary  # Use twoClassSummary to compute ROC
)

# Train the XGBoost model using caret
set.seed(100)
xgb_model <- train(
  Diagnose ~ ., 
  data = trainData,
  method = "xgbTree",
  trControl = train_control,
  metric = "ROC",                 # Use ROC as the metric
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  )
)

# Print the model details
preds <- predict(xgb_model, newdata = testData)

# Get predicted probabilities
pred_probs <- predict(xgb_model, newdata = testData, type = "prob")

# Calculate the confusion matrix
conf_matrix <- confusionMatrix(preds, testData$Diagnose,positive = "X1")

# Print the confusion matrix and other metrics
print(conf_matrix)




# resample() ile modellerin karsilastirilmasi
models_compare.1 <- resamples(list(LOGREG= model_log, 
                                   LDA=model_lda,
                                   DECISIONTREE=model_tree,
                                   NAIVEBAYES=model_nb,
                                   GBM=model_gbm,
                                   ADABOOST=model_adaboost,
                                   SVM=svm_model
                                   
                                   
                                   
))

# Model performanslarinin ozetlenmesi 
summary(models_compare.1)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare.1, scales=scales)


# Model performanslarinin ozetlenmesi 

#MODASDASDIHA



fitted <- predict(svm_model, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="X1")

fitted <- predict(model_lda, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="X1")

fitted <- predict(model_gbm, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="X1")

fitted <- predict(model_adaboost, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="X1")


fitted <- predict(model_log, testData)
confusionMatrix(reference = testData$Diagnose, data = as.factor(fitted), mode='everything', positive="X1")



varimp_ada <- varImp(model_adaboost)
plot(varimp_ada, main="adaboost icin degisken onem duzeyleri")

varimp_svm <- varImp(svm_model)
plot(varimp_svm, main="svm icin degisken onem duzeyleri")

varimp_xgb<-varImp(xgb_model)
plot(varimp_xgb, main="xgb icin degisken onem duzeyleri")
varimp_log <- varImp(model_log)
plot(varimp_log, main="LOGREG icin degisken onem duzeyleri")






























