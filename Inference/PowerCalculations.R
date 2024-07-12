#Statistical power
library(dplyr)

dat=mice_pheno
chow=filter(dat,Sex=='F'& Diet=='chow')%>%select(Bodyweight)%>%unlist()
hf=filter(dat,Sex=='F'&Diet=='hf')%>%select(Bodyweight)%>%unlist()
(mu_hf=mean(hf))
(mu_cont=mean(chow))
print(mu_hf-mu_cont)
print(mu_hf-mu_cont)/mu_cont*100#percent increase
set.seed(1)
N=5
hfn=sample(hf,N)
chown=sample(chow,N)
t.test(hfn,chown)$p.value
#Veri seti düzgün çalışmıyor, ancak tip iki hatadan bahsediyor
#total veri setiyle örneklem almadan yaptığında alternatif hipotezi
#doğru olduğunu biliyoruz
#ancak az örnekleme yapınca alternatif hipotezi red ediyoruz 
#bu tip iki hata olur yetersiz örneklemden dolayı alternatif hipotez
#doru olmasına rağmen reddetdik.
alpha=0.05
b=2000
reject=function(N,alpha=0.05){
  hfn=sample(hf,N)
  chown=sample(chow,N)
pval=t.test(hfn,chown)$p.value
pval<alpha
}
reject(12)
rejections=replicate(b,reject(N))
mean(rejections)
Ns=seq(5,50,5)
power=sapply(Ns,function(N){
  rejections=replicate(b,reject(N))
  mean(rejections)
})
plot(Ns,power,type = "b")
N=30
#Power calculations exercies
library(downloader)
,url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
#Set the seed at 1, then use the replicate() function to repeat the code used in the exercise above 
#10,000 times. What proportion of the time do we reject at the 0.05 level?
set.seed(1)
N=5
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)




























