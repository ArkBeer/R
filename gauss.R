
gauss<-function(m){
  
  gauss_impl<-function(m){
    for(j in c(1:(length(m[,1])-1))){
      for(i in c((j+1):length(m[,1]))){
        m[i,]<-m[i,]-m[j,]*m[i,j]/m[j,j]
      }
    }
    ans<-rev(m[,length(m[1,])])
    m<-matrix(rev(m[,-length(m[1,])]),length(m[,1]),length(m[,1]))
    return(cbind(m,ans))
  }
  
  m<-gauss_impl(m)
  m<-gauss_impl(m)
  for(i in c(1:length(m[,1]))){
    m[i,]<-m[i,]/m[i,i]
  }
  return(m)
}

m<-matrix(c(1,2,2,2,2,2,3,3,1),3,3)
ans<-c(2,1,-1)

t<-proc.time()
m<-gauss(cbind(m,ans))
t<-proc.time()-t

#or system.time(gauss(cbind(m,ans)))