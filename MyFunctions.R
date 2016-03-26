myFunction <- function(x) {
  
  y <- rnorm(100)
  mean(y)
}

second <- function(x){
  
  x+ rnorm(length(x))
}


y<-subset.matrix(x,x$Ozone>31 & x$Temp>90)

mean(y[,2])


good<-complete.cases(y[,1])
y[good,]
as.data.frame(y[good,2])

y[good,1]

max(y[good,1])



library(swirl)