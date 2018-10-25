library(tidyverse)
N<-4
x<-matrix(c(5,4,1,1,4,5,1,1,1,1,4,2,1,1,2,4),N,N)
jacobi<-function(x){
  n<-NROW(x)
  v<-diag(n)
  while(TRUE){
    c<-x %>% abs() %>% replace(upper.tri(.,diag=TRUE),0) %>% which.max()
    p<-ifelse(c%%n!=0,as.integer(c/n)+1,as.integer(c/n))
    q<-ifelse(c%%n!=0,c%%n,4)
    theta<-atan(-2*x[p,q]/(x[p,p]-x[q,q]))/2
    u<-diag(n)
    u[p,p]<-u[q,q]<-cos(theta)
    u[q,p]<-(-sin(theta))
    u[p,q]<-(sin(theta))
    v<-v%*%u
    x2<-x<-t(u) %*% x %*% u
    diag(x2)<-0
    if(sqrt(sum(x2*x2))/n<=10^-6)break
  }
  return(list(x,v))
}
