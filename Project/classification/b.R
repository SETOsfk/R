# Load necessary packages
install.packages("caret")
install.packages("DMwR2")
library(caret)
library(DMwR2)



# Define the training control with SMOTE
train_control <- trainControl(method = "cv", number = 5, sampling = "smote", 
                              summaryFunction = twoClassSummary, classProbs = TRUE)

# Define the metric for model evaluation
metric <- "ROC"

# Define a grid of hyperparameters for each model
tune_grid <- expand.grid(
  nrounds = c(10046, 51),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 0.1, 0.2),
  colsample_bytree = c(0.6, 0.8, 1.0),
  min_child_weight = c(1, 5, 10),
  subsample = c(0.6, 0.8, 1.0)
)
# Train GLM model
model_glm <- train(Diagnose ~ ., data =trainData, method = "glm", 
                   trControl = train_control, metric = metric)
model_glm
# Train GBM model
model_gbm <- train(Diagnose ~ ., data = trainData, method = "gbm", 
                   trControl = train_control, metric = metric)
model_gbm
# Train SVM model
model_svm <- train(Diagnose ~ ., data = trainData, method = "svmRadial", 
                   trControl = train_control, metric = metric)
model_svm

# Train XGBoost model

# Train AdaBoost model
model_ada <- train(Diagnose ~ ., data = trainData, method = "AdaBoost.M1", 
                   trControl = train_control, metric = metric)
model_ada
# Compare models
results <- resamples(list(AdaBoost = model_ada, GLM = model_glm, GBM = model_gbm, 
                          SVM = model_svm))
summary(results)
dotplot(results)

# Print the best model based on ROC
print(model_xgb)
