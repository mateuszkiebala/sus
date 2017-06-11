
ex1=data.frame(a0=c(1,1,1), a1=c(1,2,3), d=c(2,1,2))
ex2=data.frame(a0=c(1,1,1), a1=c(1,2,3), d=c(0,1,2))

lin_reg = function(df) {
  a=df[paste("a", 0:(length(df)-2), sep="")]
  d=df$d
  m=matrix(0,length(a),length(a))
  b=rep(0,length(a))
  for (k in 1:length(a)) 
    for (j in 1:length(d)) {
      for (i in 1:length(a)) 
        m[k,i] = m[k,i] + a[j,i]*a[j,k]
      b[k] = b[k] + d[j]*a[j,k]
      }
  w=solve(m,b)
  names(w)=paste("w", 0:(length(a)-1), sep="")
  w
  }

cl.value = function(x,w)
  sum(x*w)

test.df = function(min_a,max_a,len,w) {
  df=data.frame(a0=rep(1,len),a1=seq(from=min_a,to=max_a,len=len),d=rep(0,len))
  for(i in 1:len)
    df$d[i]=cl.value(c(1,df$a1[i]),w)
  df
  }
  
do.test = function(df,min_a1,max_a1) {
  w = lin_reg(df)
  test = test.df(min_a1,max_a1,200,w)
  plot(test$a1,test$d,type="l")
  points(df$a1,df$d)
  }

#do.test(ex1,0,4)
#do.test(ex2,0,4)
  
ex3=data.frame(a1=c(1,2,3), d=c(2,1,2))
ex4=data.frame(a1=c(1,2,3), d=c(0,1,2))

kernel.reg = function(df,K,n) {
  a=df[paste("a", 1:(length(df)-1), sep="")]
  d=df$d
  m=matrix(0,length(d),length(d))
  b=rep(0,length(d))
  for (k in 1:length(d)) 
    for (j in 1:length(d)) {
      for (l in 1:length(d)) 
        m[k,l] = m[k,l] + K(a[l,],a[j,],n)*K(a[k,],a[j,],n)
      b[k] = b[k] + d[j]*K(a[k,],a[j,],n)
      }
  v=solve(m,b)
  names(v)=paste("v", 1:length(d), sep="")
  v
  }

kernel.reg2 = function(df,K,n) {
  a=df[paste("a", 1:(length(df)-1), sep="")]
  d=df$d
  m=matrix(0,length(d),length(d))
  b=rep(0,length(d))
  for (k in 1:length(d)) {
    for (l in 1:length(d)) 
      m[k,l] = m[k,l] + K(a[l,],a[k,],n)
    b[k] = d[k]
    }
  v=solve(m,b)
  names(v)=paste("v", 1:length(d), sep="")
  v
  }

linear.kernel = function(x,y,n)
  sum(x*y)
  
m.polynomial.kernel = function(x,y,n)
  sum(x*y)^n
  
polynomial.kernel = function(x,y,n)
  (sum(x*y)+1)^n
  
gaussian.kernel = function(x,y,n)
  exp(-sum((x-y)^2)/(n^2))
  
kernel.cl.value = function(df,K,n,x,v) {
  a=df[paste("a", 1:(length(df)-1), sep="")]
  y=0  
  for (l in 1:length(v)) 
    y=y+v[l]*K(a[l,],x,n)
  y
  }
  
kernel.test.df = function(train.df,min_a,max_a,len,K,n,v) {
  df=data.frame(a1=seq(from=min_a,to=max_a,len=len),d=rep(0,len))
  for(i in 1:len)
    df$d[i]=kernel.cl.value(train.df,K,n,df$a1[i],v)
  df
  }
  
do.kernel.test = function(df,min_a1,max_a1,K,n) {
  v=kernel.reg(df,K,n)
  test = kernel.test.df(df,min_a1,max_a1,200,K,n,v)
  plot(test$a1,test$d,type="l")
  points(df$a1,df$d)
  }
  
do.kernel.test2 = function(df,min_a1,max_a1,K,n) {
  v=kernel.reg2(df,K,n)
  test = kernel.test.df(df,min_a1,max_a1,200,K,n,v)
  plot(test$a1,test$d,type="l")
  points(df$a1,df$d)
  }
  
#do.kernel.test(ex3,-1,4,polynomial.kernel,2)
#do.kernel.test(ex4,-1,4,polynomial.kernel,2)
 
ex5=data.frame(a1=c(1,2,3,4,5), d=c(1,0,2,-1,3))
#do.kernel.test2(ex5,0,6,polynomial.kernel,4)
#do.kernel.test2(ex5,0,6,gaussian.kernel,1)
#do.kernel.test2(ex5,0,6,gaussian.kernel,10)
#do.kernel.test2(ex5,0,6,gaussian.kernel,5)
#do.kernel.test2(ex5,0,6,gaussian.kernel,2)
#do.kernel.test2(ex5,0,6,gaussian.kernel,0.5)
#do.kernel.test2(ex5,0,6,gaussian.kernel,0.1)

reg.kernel.reg = function(df,K,n,gamma) {
  a=df[paste("a", 1:(length(df)-1), sep="")]
  d=df$d
  m=matrix(0,length(d),length(d))
  b=rep(0,length(d))
  for (k in 1:length(d)) {
    for (l in 1:length(d)) 
      m[k,l] = m[k,l] + K(a[l,],a[k,],n)
    m[k,k] = m[k,k] + gamma*length(d)
    b[k] = d[k]
    }
  v=solve(m,b)
  names(v)=paste("v", 1:length(d), sep="")
  v
  }

do.reg.kernel.test = function(df,min_a1,max_a1,K,n,gamma) {
  v=reg.kernel.reg(df,K,n,gamma)
  test = kernel.test.df(df,min_a1,max_a1,200,K,n,v)
  plot(test$a1,test$d,type="l")
  points(df$a1,df$d)
  }
  
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,4,0)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,4,1)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,4,10)
#do.reg.kernel.test(ex5,0,6,gaussian.kernel,1,0)
#do.reg.kernel.test(ex5,0,6,gaussian.kernel,1,10)
#do.reg.kernel.test(ex5,0,6,gaussian.kernel,1,1)
#do.reg.kernel.test(ex5,0,6,gaussian.kernel,1,0.1)
#do.reg.kernel.test(ex5,0,6,gaussian.kernel,1,0.01)
#do.reg.kernel.test(ex5,0,6,gaussian.kernel,1,0.001)
#do.reg.kernel.test(ex5,0,6,gaussian.kernel,1,0.0001)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,4,0)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,4,10)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,4,1)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,4,0.1)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,4,0.01)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,4,0.001)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,4,0.0001)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,2,0)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,2,10)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,2,1)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,2,0.1)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,2,0.01)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,2,0.001)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,2,0.0001)
#do.reg.kernel.test(ex5,0,6,polynomial.kernel,2,0.00001)

ex6=data.frame(a1=c(1,2,3,4,5), d=c(1,1,1,1,1))
#do.reg.kernel.test(ex6,0,6,polynomial.kernel,2,0)
#do.reg.kernel.test(ex6,0,6,polynomial.kernel,2,10)
#do.reg.kernel.test(ex6,0,6,polynomial.kernel,2,1)
#do.reg.kernel.test(ex6,0,6,polynomial.kernel,2,0.1)
#do.reg.kernel.test(ex6,0,6,polynomial.kernel,2,0.01)
#do.reg.kernel.test(ex6,0,6,polynomial.kernel,2,0.001)
#do.reg.kernel.test(ex6,0,6,polynomial.kernel,2,0.0001)
#do.reg.kernel.test(ex6,0,6,polynomial.kernel,1,0.0001)
#do.reg.kernel.test(ex6,0,6,polynomial.kernel,1,0.0)
#do.reg.kernel.test(ex6,0,6,polynomial.kernel,1,0.001)
#do.reg.kernel.test(ex6,0,6,polynomial.kernel,1,0.01)

#przykład z sin(1/x)

#eliminacja szumu

#przykłady z dwoma wymiarami:
#image(x, y, z, ...)
#contour(x, y, z, ...)
#persp(x, y, z, ...)

#co się dzieje przy wielu (np 100) wymiarach?

#jądro z funkcji giętych

#zmiana funkcjonału błędu na normę supremum i regularyzatora na normę liczącą
#i znajdowanie klasyfikatora za pomocą hill-climbing

