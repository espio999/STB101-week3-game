library(e1071)

makeVector=function(x, myTitle) {
  pts.names=c(
    "Mean",
    "Median",
    "Mode",
    "Standard deviation",
    "Standard error",
    "Variance",
    "Kurtosis",
    "Skewness",
    #"Range",
    "Min",
    "Max",
    "Total",
    "Length"
  )
  
  pts=c(
    mean(x),
    median(x),
    names(which.max(table(x))),
    sd(x),
    sd(x)/sqrt(length(x)),
    var(x),
    kurtosis(x),
    skewness(x),
    #range(x),
    min(x),
    max(x),
    sum(x),
    length(x)
  )
  
  names(pts)=pts.names
  
  return(pts)
}

drawChart=function(val, myStr){
  par(mfcol=c(3,1))
  
  #myHist=hist(val, breaks="Scott", main=myStr)
  myHist=hist(val, breaks="Sturges", main=myStr)
  
  boxplot(val, horizontal=T)
  par(new=T)
  stripchart(val, method="stack")
  
  myHist$counts=cumsum(myHist$counts)
  plot(myHist, axes=F, main=myStr)
  par(new=T)
  plot(sort(val), 1:length(val))
}

age = week3$Age
print(makeVector(age))
drawChart(age, "age")

sales = week3$Sales
print(makeVector(sales))
drawChart(sales, "sales")

plot(age, sales, main="age-sales")