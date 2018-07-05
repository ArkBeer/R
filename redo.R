library(tidyverse)
jacobi<-function(x,A,b){
  x<-1:length(A[,1]) %>% 
    map(~{(b[.]-sum(x * A[.,])+x[.]*A[.,.])/A[.,.]}) %>% 
    unlist()
}
gauss_seidel<-function(x,A,b){
  x<-1:length(A[,1]) %>% 
    map(~{x[.]<<-(b[.]-sum(x * A[.,])+x[.]*A[.,.])/A[.,.]}) %>% 
    unlist()
}
sor<-function(x,A,b,omega){
  tilde<-gauss_seidel(x,A,b)
  x+omega*(tilde-x)
}
# A<-matrix(runif(9),3,3)
# b<-c(runif(3))
# x<-c(runif(3))
# x2<-x

A<-matrix(c(3,1,1,1,3,1,1,1,3),3,3)
b<-c(0,4,6)
x<-c(runif(3))
x2<-x
x2<- x2 %>% jacobi(A,b)
x2<- x2 %>% gauss_seidel(A,b)
x2<- x2 %>% sor(A,b,1.9)