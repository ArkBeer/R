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
  return(matrix(c(dx(f,x,y),dx(g,x,y),dy(f,x,y),dy(g,x,y)),2,2))
}


newton<-function(f,g,x0,y0){
  c0<-matrix(c(x0,y0),2,1)
  while(T){
    print(c0)
    c1<-c0-solve(jacobi(f,g,x,y),matrix(c(f(c0[1],c0[2]),g(c0[1],c0[2])),2,1))
    if(abs(f(c1[1],c1[2]))<10^-6 & abs(g(c1[1],c1[2]))<10^-6){
      break
    }
    c0<-c1
  }
}
