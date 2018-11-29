library(tidyverse)
x<-matrix(c(2,2,1,1,2,2,2,1,2,1,1,1,2,2,1,1,2,2,2,1,2,1,1,1,2),5,5)
x<-matrix(runif(25),5,5)
x<-x+t(x)

householder<-function(x){
  A<-x
  for(i in 1:(NROW(A)-2)){
    z<-A[(i+1):NROW(A),i]
    s<-sqrt(sum(z*z))
    e<-numeric(NROW(A)-i)
    e[1]<-1
    u<-(z-s*e)/sqrt(sum((z-s*e)*(z-s*e)))
    v<-c(numeric(i),u)
    p<-diag(NROW(A))-2*v%*%t(v)
    A<-p%*%A%*%p
  }
  return(A)
}
