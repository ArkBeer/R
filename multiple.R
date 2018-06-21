mul<-function(x,y){
  if(length(x[1,])!=length(y[,1]))return(FALSE)
  temp<-c()
  for(j in 1:length(y[1,])){
    for(i in 1:length(x[,1])){
      temp<-c(temp,sum(x[i,]*y[,j]))
    }
  }
  return(matrix(temp,length(x[,1]),length(y[1,])))
}
x<-matrix(c(1:12),4,3)
y<-matrix(c(1:12),3,4)

result<-mul(x,y)