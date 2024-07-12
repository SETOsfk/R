library(dplyr)
dat=femaleMiceWeights
control=filter(dat,Diet=='chow')%>%select(Bodyweight)%>%unlist()
treatment=filter(dat,Diet=='hf')%>%select(Bodyweight)%>%unlist()
#On average hf ler chowlardan daha ağır
mean(treatment)-mean(control)
#şu an tüm popülasyona sahibiz ama normalde olmaz bu
population=unlist(femaleControlsPopulation)
#Her basışında farklı bir ortalama çıkar.
mean(sample(population,12))
#Nıll distrubition, p-values
obs=mean(treatment)-mean(control)
#Eğer sıfır hipotezi doğruysa aralarında fark yok demektir.
control=sample(population,12)
treatment=sample(population,12)
mean(treatment)-mean(control)
#Her yaptığında farklı değerler alırsın şimdi bunu for loopuna sokup
#10000 tane farklı değeri kaydedicez.
n=10000
nulls=vector("numeric",n)
for(i in 1:n){
  control=sample(population,12)
  treatment=sample(population,12)
  nulls[i]=mean(treatment)-mean(control)
  
}
max(nulls)
min(nulls)
hist(nulls)
#Normal dağıldığını görüyorsun
(mean(abs(nulls)>obs))#P valuen bu senin. Populasyon ortalamasının örneklem orttan büyük olduğu zamanlar
#Bu örneklerim kaçında sıfır hipotezi obsden büyüktü.