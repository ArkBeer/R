library(tidyverse)
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

count_change_of_sign<-function(B,lambda){
  p<-c(1,lambda-B[1,1])
  for(i in 2:NROW(B)){
    p<-c(p,(lambda-B[i,i])*rev(p)[1]-B[i-1,i]^2*rev(p)[2])
  }
  m<-0
  for(i in 2:(NROW(B)+1)){
    if(p[i-1]*p[i]<0){
      m<-m+1
    }
  }
  return(m)
}

givens<-function(B){
  lambda<-c()
  for(k in 1:NROW(B)){
    beta<-abs(B) %>% norm("i")
    alpha<-(-beta)
    while(TRUE){
      mu<-(alpha+beta)/2
      m<-count_change_of_sign(B,mu)
      if(m<k){
        beta<-mu
      }else{
        alpha<-mu
      }
      if(abs(alpha-beta)<10^-6){
        lambda<-c(lambda,alpha)
        break
      }
    }
  }
  return(lambda)
}
