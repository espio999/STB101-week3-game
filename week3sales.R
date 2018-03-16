library(e1071)

x = week3$Sales

myMean=mean(x)
myMedian=median(x)
myMode=names(which.max(table(x)))
mySd=sd(x)
mySe=mySd/sqrt(length(x))
myVar=var(x)
myKurt=kurtosis(x)
mySkew=skewness(x)
myRange=range(x)
myMin=min(x)
myMax=max(x)
myTotal=sum(x)
myLength=length(x)

hist(x,breaks=seq(0,14400,1276.2),xlab="sales")

