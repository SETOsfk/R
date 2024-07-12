install.packages("pacman")
library(pacman)
pacman::p_load( 
  MASS, 
  ggplot2, 
  dplyr, 
  tidyverse,
  corrplot,
  leaps,
  rpart,
  mgcv,
  glmnet,
  boot,
  rpart.plot
)



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
sum(duplicated(data))
index(duplicated(data))
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
par(mfrow = c(1,1))
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

#Karar ağacı kurarken bir noktada durdurmak PRUNING yapmak gerekiyor.
#Karar agaci modeli olusturma
#default value of cp = 0.01
Boston.tree <- rpart(medv ~ ., data = Boston.train)
Boston.tree
rpart.plot(Boston.tree, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)
plotcp(Boston.tree)
printcp(Boston.tree)
Boston.largetree <- rpart(formula = medv ~ ., data = Boston.train, cp = 0.001)#daha da düşsün diye yaptık hata payı

plot(Boston.largetree)
text(Boston.largetree)
printcp(Boston.largetree)
plotcp(Boston.largetree)
pruned.tree <- prune(Boston.largetree, cp = 0.0072) #sonrasında trimledik
pruned.tree
rpart.plot(pruned.tree, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE, extra = 1)
#In-sample MSE #YANİ KENDİ İÇİNDE ÖRNEKLERLE DENENDİ
mean((predict(Boston.tree) - Boston.train$medv) ^ 2)      #default tree
mean((predict(Boston.largetree) - Boston.train$medv) ^ 2)  #large tree
mean((predict(pruned.tree) - Boston.train$medv) ^ 2)       #pruned tree
#out-of-sample performance #DIŞARDAN ÖRNEKLERLE DENENDİ 
mean((predict(Boston.tree, newdata = Boston.test) - Boston.test$medv) ^ 2)  #default tree
mean((predict(Boston.largetree, newdata = Boston.test) - Boston.test$medv) ^ 2)   #large tree
mean((predict(pruned.tree, newdata = Boston.test) - Boston.test$medv) ^ 2)     #pruned tree

#spline regresyon
#KATSAYILARIN FONKSİYON OLMASINI SAĞLAR.

#GAM modeli
#model 1 - not using s() on chas and rad, leaving them as integers
Boston.gam <- gam(medv ~ s(crim) + s(zn) + s(indus) + s(nox) + s(rm) + s(age) + s(dis) + 
                    s(tax) + s(ptratio) + s(black) + s(lstat) + chas + rad, data = Boston.train)
summary(Boston.gam)
#Model AIC, BIC
AIC(Boston.gam)
BIC(Boston.gam)
#plot
plot(Boston.gam, shade = TRUE, seWithMean = TRUE, scale = 0)

#In-sample prediction
(Boston.gam.mse <- mean((predict(Boston.gam) - Boston.train$medv) ^ 2))
#Out-of-sample prediction - MSPE
(Boston.gam.mspe <- mean((predict(Boston.gam, newdata = Boston.test) - Boston.test$medv) ^ 2))
help("rpart")




#MODELLERE BAKACAZ
#HANGİ MODEL DAHA İYİ SONUÇ VERDİ
#HANGİSİNİN SONUCU NE ONU GÖSTERİCEZ


















