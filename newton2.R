f<-function(x,y){
  return(2*x-y^2+log(x))
}

g<-function(x,y){
  return(x^2-x*y-x+1)
}


dx<-function(f,x,y){
  h<-10^(-6)
  return((f(x+h,y)-f(x,y))/h)
}

dy<-function(f,x,y){
  h<-10^(-6)
  return((f(x,y+h)-f(x,y))/h)
}

jacobi<-function(f,g,x,y){
  return(dx(f,x,y)*dy(g,x,y)-dy(f,x,y)*dx(g,x,y))
}

newton<-function(f,g,x0,y0){
  c0<-c(x0,y0)
  while(T){
    print(c0)
    c1<-c0-c(f(c0[1],c0[2]),g(c0[1],c0[2]))/abs(jacobi(f,g,c0[1],c0[2]))*0.1
    if(abs(f(c1[1],c1[2]))<10^-6 & abs(g(c1[1],c1[2]))<10^-6){
      break
    }
    newton(f,g,c1[1],c1[2])
  }
}
