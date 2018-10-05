f<-function(x){
  return(x-exp(1)^(-x))
}
d<-function(f,x){
  h<-10^(-6)
  return((f(x+h)-f(x))/h)
}

newton<-function(f,x0){
  print(x0)
  if(abs(f(x0)-0)>=10^(-6)){
    return(newton(f,x0-f(x0)/d(f,x0)))
  }else return(x0)
}
