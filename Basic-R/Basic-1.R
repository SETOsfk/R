install.packages("swirl")
library(swirl)
#Create a numeric vector containing the numbers 
#2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23.
#What is the average of these numbers?
(v=mean(c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)))
#1 den 25'e kadarki sayıların karesini alan for loop
sum=0
for (i in 1:25){
  (sum=sum+i^2)
}
#Cars da kaç row var
nrow(cars)
#What is the name of the second column of cars?
names(cars)[2]
#The simplest way to extract the columns of a 
#matrix or data.frame is using [. For example you can 
#access the second column with cars[,2].
#What is the average distance traveled in this dataset?
(avg_dist=mean(cars[,2]))
#Familiarize yourself with the which() function. 
#Which row of cars has a a distance of 85?
which(cars[,2]==85)
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
download(url, destfile=filename)
#Read in the file femaleMiceWeights.csv 
#and report the exact name of the column containing the weights.
dat=read.csv("femaleMiceWeights.csv")
head(dat)
names(dat)[2]
#The [ and ] symbols can be used to extract specific
#rows and specific columns of the table.
#What is the entry in the 12th row and second column?
dat[12,2]
#You should have learned how to use the $ character to extract a 
#column from a table and return it as a vector. 
#Use $ to extract the weight column and report the weight of the mouse in the 11th row.
weight=c(dat$Bodyweight)
weight[11]
#To create a vector with the numbers 3 to 7, 
#we can use seq(3,7) or, because they are consecutive, 
#3:7. View the data and determine what rows are 
#associated with the high fat or hf diet. Then use the mean() 
#function to compute the average weight of these mice.
#What is the average weight of mice on the high fat diet?
dat
mean(weight[13:24])
set.seed(1)
sample(weight[13:24],1)
















