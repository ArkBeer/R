gauss<-function(m){
  
  gauss_impl<-function(m){
    for(j in c(1:(length(m[,1])-1))){
      for(i in c((j+1):length(m[,1]))){
        if(m[j,j]!=0)m[i,]<-m[i,]-m[j,]*m[i,j]/m[j,j]
      }
    }
    ans<-rev(m[,length(m[1,])])
    m<-matrix(rev(m[,-length(m[1,])]),length(m[,1]),length(m[,1]))
    return(cbind(m,ans))
  }
  
  m<-gauss_impl(m)
  m<-gauss_impl(m)
  for(i in c(1:length(m[,1]))){
    if(m[i,i]!=0)m[i,]<-m[i,]/m[i,i]
  }
  return(m)
}

#m0<-matrix(c(1,2,2,2,2,2,3,3,1),3,3)
#ans0<-c(2,1,-1)
#m<-matrix(c(2,4,2,3,4,10,6,7,2,3,1,1,2,3,1,4),4,4)
#ans<-c(8,17,9,11)
n<-1000
m0<-matrix(c(runif(n*n)),n,n)
ans0<-c(runif(n))
t<-proc.time()
m<-gauss(cbind(m0,ans0))
t<-proc.time()-t
norm(matrix(ans0,n,1)-matrix(m0%*%m[,n+1],n,1))/norm(matrix(ans0,n,1))
#or system.time(gauss(cbind(m,ans)))
