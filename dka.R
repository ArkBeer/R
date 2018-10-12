f<-function(x){
  return(x^3+2*x^2+4*x^1+1)
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

dka<-function(f,a1,n,r0){
  zk.0<-c()
  for(i in 1:n){
    zk.0<-c(zk.0,a1/n+r0*exp(1i*(2*(i-1)*pi/n+pi/2/n)))
  }
  while(TRUE){
    zk.1<-c()
    for(i in 1:n){
      zk.1<-c(zk.1,zk.0[i]-f(zk.0[i])/prod(zk.0[i]-zk.0[-i]))
    }
    print(zk.1)
    if(all(abs(zk.1-zk.0)<10^-6*abs(zk.1))){
      break
    }else{
      zk.0<-zk.1
    }
  }
  return(zk.1)
}
