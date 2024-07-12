library(dplyr)
control=filter(dat, Diet=='chow')%>%select(Bodyweight)%>%unlist()
control
library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)
dat1=read.csv("msleep_ggplot2.csv")
#Sadece primateleri seç ve row sayısını söyle
dat_primates=filter(dat1,order=='Primates')
nrow(dat_primates)
#Sleep totallarını al
dat_primates=filter(dat1,order=='Primates')%>%select(sleep_total)%>%unlist()
mean(dat_primates)
#Alternatif cevap
(dat_primatesV2=filter(dat1, order=='Primates') %>% summarize( mean( sleep_total)))
