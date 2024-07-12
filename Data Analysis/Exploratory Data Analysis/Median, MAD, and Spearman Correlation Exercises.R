data(ChickWeight)
library(dplyr)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)
chick = na.omit(chick)
chick44=chick$weight.4
chick4=c(chick44,3000)
chick21o=c(chick21,3000)
mean(chick4)/mean(chick44)
mean(c(chick$weight.4, 3000))/mean(chick$weight.4)## Satır 9:12 ile aynı işi yapıyo
median(c(chick$weight.4, 3000))/median(chick$weight.4)
sd(c(chick$weight.4, 3000))/sd(chick$weight.4)
mad(c(chick$weight.4, 3000))/mad(chick$weight.4)
chick21=chick$weight.21
chick21o=c(chick21,3000)
cor(chick4,chick21o)/cor(chick44,chick21)
#cor(c(chick$weight.4, 3000), c(chick$weight.21,3000))/cor(chick$weight.4, chick$weight.21)