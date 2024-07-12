url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist #sigara içmeyenlerin listesi
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist #sigara içenlerin listesi
library(rafalib)
library(dplyr)
mean(bwt.nonsmoke)-mean(bwt.smoke)#ortalamalarının farkını al
popsd(bwt.nonsmoke)#s içmeyen ss
popsd(bwt.smoke)#s içen ss
set.seed(1)
N=5
dat.s=sample(bwt.smoke,N)#ikisinden de 5 örneklem al
dat.ns=sample(bwt.nonsmoke,N)
#Set the seed at 1, then use the replicate() function to repeat the code used in the exercise above 10,000 times. 
#What proportion of the time do we reject at the 0.05 level?
B <- 10000
alpha <- 0.05
set.seed(1)
N <- 5
res_list<- replicate(B, {
  ns_sample <- sample(bwt.nonsmoke, N)#NS den 5 örnek al
  s_sample <- sample(bwt.smoke, N)#S den 5 örnek al 
  pval <- t.test(ns_sample, s_sample)$p.value # t teslerindeki p değerini kaydet BUNU 10.000 defa yap ve
  return(alpha > pval) # this returns logical (i.e., TRUE if pval is smaller than 0.05 = alpha), and stores in the vector res_list.
  #alpha değeri olan 0.05in bulduğumuz p değerinden büyük olduğu aralıkları al.
})
mean(res_list) #bu testin güvenilirliği %10 civarı?? tam emin olamadım burdan.
#Note that, not surprisingly, the power is lower than 10%. Repeat the exercise above for samples sizes of 30, 60, 90 and 120.
#Which of those four gives you power of about 80%?
B <- 10000
alpha <- 0.05
set.seed(1)
N <- 60
res_list<- replicate(B, {
  ns_sample <- sample(bwt.nonsmoke, N)
  s_sample <- sample(bwt.smoke, N)
  pval <- t.test(ns_sample, s_sample)$p.value
  return(alpha > pval) # this returns logical (i.e., TRUE if pval is smaller than 0.05 = alpha), and stores in the vector res_list.
})
mean(res_list) 
#Repeat the problem above, but now require an  level of 0.01. 
#Which of those four gives you power of about 80%?
B <- 10000
alpha <- 0.01
set.seed(1)
N <- 90
res_list<- replicate(B, {
  ns_sample <- sample(bwt.nonsmoke, N)
  s_sample <- sample(bwt.smoke, N)
  pval <- t.test(ns_sample, s_sample)$p.value
  return(alpha > pval) # this returns logical (i.e., TRUE if pval is smaller than 0.01 = alpha), and stores in the vector res_list.
})
mean(res_list)

  