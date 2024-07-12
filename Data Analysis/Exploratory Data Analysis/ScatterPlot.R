library(UsingR)
#Babaların boyu çocukların boyunu etkiler mi?
data("father.son")
x=father.son$fheight
y=father.son$sheight
plot(x,y,xlab="Father'S height in inches",ylab="Son's height in inches",
main = paste("correlation =",signif(cor(x,y),2)))
#Korelasyon aldık.
#Peki ya babasının boyu 72 olan çocuğun boyu sorulsa?
boxplot(split(y,round(x)))#Grup oluşturuyoruz babaların uzunluklarını en yakın inche yuvararyıp
#çocukları da oraya grupluyoruz yani mesela 71.8 olan bir baba 72 ye yuvarlanıcak ve onun çocuğu da 72 grubunda bulunucak
print(mean(y[round(x)==72]))
#Standarlaştıralım datayı
x=(x-mean(x))/sd(x)
y=(y-mean(y))/sd(y)
means=tapply(y,round(x*4)/4,mean)#yine grupladık
fatherhh=as.numeric(names(means))
plot(fatherhh,means,ylab="average of starate of son heights",ylim=range(fatherhh))
###EXERCISE
data(nym.2002, package="UsingR")
library(dplyr)
m=filter(nym.2002,gender=="Male")
f=filter(nym.2002,gender=="Female")
maleAGE=m$age
maleTime=m$time
cor(maleTime,maleAGE)
fAGE=f$age
fTime=f$time
cor(fTime,fAGE)
#What is the fastest time divided by the median time
time = sort(nym.2002$time)
fasttime=min(time)
fasttime/median(time)
#What is the slowest time divided by the median time?
lowtime=max(time)
lowtime/median(time)
