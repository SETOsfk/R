plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
chick = na.omit(chick)
library(dplyr)
x=filter(chick,Diet==1)%>%select(weight.4)%>%unlist()
y=filter(chick,Diet==4)%>%select(weight.4)%>%unlist()
t.test(x,y)
wilcox.test(x,y)
xO=c(x,200)
t.test(xO,y)$p.value
wilcox.test(xO,y)
library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)
t.test(x,y+10)$statistic-t.test(x,y+100)$statistic
t.test(x,y+100)$statistic
        
