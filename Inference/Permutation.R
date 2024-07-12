library(dplyr)
library(rafalib)
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)#içmeyenlerden N örnek
smokers <- sample(bwt.smoke , N)#içenlerden N örnek
obsdiff <- mean(smokers) - mean(nonsmokers)#ortalama farkları

avgdiff=replicate(1000,{
dat <- c(smokers,nonsmokers)
shuffle <- sample( dat )
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
return(mean(smokersstar)-mean(nonsmokersstar))
})#1000 defa tekrar et bunu
(sum(abs(avgdiff) > abs(obsdiff)) + 1) / (length(avgdiff) + 1)#toplamı

set.seed(1)
obs <- median(smokers) - median(nonsmokers)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 )
## As before we add 1 to avoid p-value of 0 but we also accept
( sum( abs(null) >= abs(obs)) ) / ( length(null) )
#https://learning.edx.org/course/course-v1:HarvardX+PH525.1x+2T2022/block-v1:HarvardX+PH525.1x+2T2022+type@sequential+block@2f5e61303d564f2597fb2afbbdaaa60d/block-v1:HarvardX+PH525.1x+2T2022+type@vertical+block@9df26e6b20e848d48452c275c6543f34
