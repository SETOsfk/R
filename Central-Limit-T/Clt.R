 #CLT central limit teorem
library(downloader) 
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
#We will remove the lines that contain missing values:
dat <- na.omit( dat )
x <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
mean(x)
library(rafalib)
popsd(x)#standart sapması
set.seed(1)
X <- sample(x,25)#xten 25 tane örneklem al
mean(X)#25 örneklemin ortalaması
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(y)
popsd(y)
set.seed(1)
Y <- sample(y,25)
mean(Y)#25 örneklemli y nin ortalaması.
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )# mutlak değer(popülasyonun farkı-25 örneklemin farkı)
##CLT
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )
pnorm(1)-pnorm(-1)#verilerin %68 civarı bu aralıkta
pnorm(2)-pnorm(-2)#95 civarı
pnorm(3)-pnorm(-3)#99 civarı
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <=1 )
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
