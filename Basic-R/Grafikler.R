library(UsingR)
x=father.son$fheight
round(sample(x,20),1)
#Breaks aralığı belirliyor nereye çizeceğini
hist(x,breaks = seq(floor(min(x)),ceiling(max(x))))
main="Height histogram",xlab="Height inches"
#########
###QQ-PLOT
#Eğer verin normal dağılıyorsa 70ten büyüklerin ortalaması aşağıdakne 
#yakın olucak
#70 yerine farklı sayılar da deneyebilirsin aradaki değerleri görürsün
mean(x>70)
1-pnorm(70,mean(x),sd(x))
##
ps=seq(0.01,0.99,0.01)
qs=quantile(x,ps)
normalqs=qnorm(ps,mean(x),sd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height")
abline(0,1)
##Daha pratiği
qqnorm(x)
qqline(x)
######
##BoxPlot
#Normal dağılmıyorsa hist ve qq iyi bir seçenek değil
hist(exec.pay)
qqnorm(exec.pay)
qqline(exec.pay)
##
boxplot(exec.pay,ylab="10,000s of dollars",ylim=c(0,400))
#Koyu çizgi medyan kutunun üstü ve altı %25-%75 arasındaki veriler
#Box plotın help fileını da oku bi outlierları güzel açıklıyor.
