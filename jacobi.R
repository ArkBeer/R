library(tidyverse)
n<-4
theta<-0
x<-matrix(c(5,4,1,1,4,5,1,1,1,1,4,2,1,1,2,4),n,n)

c<-x %>% abs() %>% replace(upper.tri(.,diag=TRUE),0) %>% which.max()
i<-as.integer(c/n)+1
j<-c%%n
