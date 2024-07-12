#MicePheno CLT
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )
#What proportion of these numbers are within one-two-three 
#standard deviations away from the list's average?
#Basicly normal dağılımda elindeki değelerin %99u
#3 ve -3 std arasında.
pnorm(1)-pnorm(-1)
pnorm(2)-pnorm(-2)
pnorm(3)-pnorm(-3)
#Define y to be the weights of males on the control diet.
#What proportion of the mice are within one-two-three standard deviation away from the average weight?
library(rafalib)
library(dplyr)
y=dat%>%filter(Sex=='M'&Diet=='chow')%>%select(Bodyweight)%>%unlist()
z <- ( y - mean(y) ) / popsd(y)
mean(abs(z)<=1)
mean(abs(z)<=2)
mean(abs(z)<=3)
set.seed(1)
avgs=replicate(10000, mean(sample(y,25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
popsd(avgs)
#https://learning.edx.org/course/course-v1:HarvardX+PH525.1x+2T2022/block-v1
#:HarvardX+PH525.1x+2T2022+type@sequential+block@d27d8d24b8c443c3acb9e8ac2877f99e/
#block-v1:HarvardX+PH525.1x+2T2022+type@vertical+block@14323fa8bd2246f78b0b51418d670ba8

