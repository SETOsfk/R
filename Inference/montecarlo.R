set.seed(1)
library(rafalib)
dat=mice_pheno
controlpop=femaleControlsPopulation%>%unlist()
#Monte carlo simülasyonu kurucaz, t değerinin yaklaşımını görücez
ttestgenerator=function(n){
  cases=sample(controlpop,n)
  controls=sample(controlpop,n)
  tstat=(mean(cases)-mean(controls))/
    sqrt(var(cases)/n+var(controls)/n)
  return(tstat)
}
#fonksiyon yarattık tstatını veren
#Şİmdi buna loopa sokalım
ttests=replicate(1000,ttestgenerator(10))
#1000 defa 10 örneklemle test et
hist(ttests)
#BU BİZİM MONTE CARLO SİMÜLASYONUMUZ.
qqnorm(ttests)
abline(0,1)
#Sample sizeın 10 iken CLT bu veri setinde gayet iyi çalışıyor
ttests=replicate(1000,ttestgenerator(3))
hist(ttests)
qqnorm(ttests)
abline(0,1)
#Örneklem sayısı düşünce etkisini kaybedebiliyor.
ps=(seq(0,999)+0.5)/1000
qqplot(qt(ps,df=2*3-2),ttests,xlim=c(-6,6),ylim=c(-6,6))
abline(0,1)
#Bu modelle de simüklasyonu test edebiliriz.
#Ancak şu an popülasyona sahip olduğumuz için bunu söyleyebiliyoruz
#Normalde popülasyonun verisi elinde olmaz
#Bunun yerine şunu yaparsın
#PC İLE popülasyon verisini yaratırsın.
controls=rnorm(5000,mean=24,sd=3.5)
#Elindeki bilgilerle popülasyona sahip değilsen yaratırsın.
ttestgenerator=function(n,mean=24,sd=3.5){
  cases=rnorm(n,mean,sd)
  controls=rnorm(n,mean,sd)
  tstat=(mean(cases)-mean(controls))/
    sqrt(var(cases)/n+var(controls)/n)
  return(tstat)
}
ttests=replicate(1000,ttestgenerator(10))
hist(ttests)
#İki tane monte carlo simülasyonu gördün, ikiside işlevsel.
#Exercise
set.seed(1)
deneme=rnorm(n=5)
deneme
sqrt(length(sample))*(mean(deneme))/sd(deneme)


set.seed(seed=1)
b=1000
mc <- replicate(1000,t.test(rnorm(n=5))$statistics)
(length(which(mc>2))/1000)
 ##
set.seed(seed=1)
mc <- replicate(1000, t.test(rnorm(n=5))$statistic)
hist(mc)
mean(mc)
sd(mc)
length(which(mc > 2)) / 1000
##
set.seed(1)
N <- 5
B<- 1000

tstats <- replicate(B,{
  X <- rnorm(N)
  sqrt(N)*mean(X)/sd(X)
})
mean(tstats>2)





















