library(MASS)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(leaps)
library(rpart)
library(mgcv)
library(glmnet)
library(boot)
library(rpart.plot)

#Veriyi tanima adimlari
data(Boston)
dim(Boston)
head(Boston)
#Degisken olcum duzeyleri
glimpse(Boston)
#tanimlayici istatistikler
summary(Boston)
#Kayip g�zlem var mi?
sum(is.na(Boston))
#tekrarlanan gozlem var mi?
sum(duplicated(Boston))

#degiskenler arasi korelasyonlar nasil?
corrplot(cor(Boston), method = "number", type = "upper", diag = FALSE)
#Veri g�rsellestirme
Boston %>%
  gather(key, val, -medv) %>%
  ggplot(aes(x = val, y = medv)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, col = "blue") +
  facet_wrap(~key, scales = "free") +
  theme_gray() +
  ggtitle("Scatter plot of dependent variables vs Median Value (medv)") 
#Veriyi egitim ve test icin parcalara ayirma
set.seed(12383010)
index <- sample(nrow(Boston), nrow(Boston) * 0.80)
Boston.train <- Boston[index, ]
Boston.test <- Boston[-index, ]
#Model kurma
model1 <- lm(medv ~ ., data = Boston.train)
model1.sum <- summary(model1)
model1.sum
#indus ve age degiskenleri olmadan kurulan model
model2 <- lm(medv ~ . -indus -age, data = Boston.train)
model2.sum <- summary(model2)
model2.sum
#best subset regression
model.subset <- regsubsets(medv ~ ., data = Boston.train, nbest = 1, nvmax = 13)
summary(model.subset)
plot(model.subset, scale = "bic")
#stepwise regression
nullmodel <- lm(medv ~ 1, data = Boston.train)
fullmodel <- lm(medv ~ ., data = Boston.train)
#forward selection
model.step.f <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "forward")
summary(model.step.f)
#Backward selection
model.step.b <- step(fullmodel, direction = "backward")
summary(model.step.b)
#stepwise selection
model.step <- step(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = "both")
summary(model.step)
#Varsayimlarin kopntrol� (model step) 
par(mfrow = c(2,2))
plot(model.step)
#In-sample performance
#MSE
model.sum <- summary(model1)
(model.sum$sigma) ^ 2
model2.sum <- summary(model2)
(model2.sum$sigma) ^ 2
#R-squared
model1.sum$r.squared
model2.sum$r.squared
#Adjusted r square
model1.sum$adj.r.squared
model2.sum$adj.r.squared
#AIC 
AIC(model1)
AIC(model2)
#BIC
BIC(model1)
BIC(model2)
#test hatasi (MSPE)
model1.pred.test <- predict(model1, newdata = Boston.test)
model1.mspe <- mean((model1.pred.test - Boston.test$medv) ^ 2)
model1.mspe
model2.pred.test <- predict(model2, newdata = Boston.test)
model2.mspe <- mean((model2.pred.test - Boston.test$medv) ^ 2)
model2.mspe
#Cross Validation
model1.glm = glm(medv ~ ., data = Boston)
cv.glm(data = Boston, glmfit = model1.glm, K = 5)$delta[2]
model2.glm <- glm(medv ~ . -indus -age, data = Boston)
cv.glm(data = Boston, glmfit = model2.glm, K = 5)$delta[2]