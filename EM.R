x <- dat

# initial values
pi1<-0.5
pi2<-0.5
mu1<--0.01
mu2<-0.01
sigma1<-sqrt(0.01)
sigma2<-sqrt(0.02)
loglik<- rep(NA, 1000)
loglik[1]<-0
loglik[2]<-mysum(pi1*(log(pi1)+log(dnorm(dat,mu1,sigma1))))+mysum(pi2*(log(pi2)+log(dnorm(dat,mu2,sigma2))))

mysum <- function(x) {
  sum(x[is.finite(x)])
}
logdnorm <- function(x, mu, sigma) {
  mysum(sapply(x, function(x) {logdmvnorm(x, mu, sigma)}))  
}
tau1<-0
tau2<-0
#k<-1
k<-2

# loop
while(abs(loglik[k]-loglik[k-1]) >= 0.00001) {
  # E step
  tau1<-pi1*dnorm(dat,mean=mu1,sd=sigma1)/(pi1*dnorm(x,mean=mu1,sd=sigma1)+pi2*dnorm(dat,mean=mu2,sd=sigma2))
  tau2<-pi2*dnorm(dat,mean=mu2,sd=sigma2)/(pi1*dnorm(x,mean=mu1,sd=sigma1)+pi2*dnorm(dat,mean=mu2,sd=sigma2))
  tau1[is.na(tau1)] <- 0.5
  tau2[is.na(tau2)] <- 0.5
  
  # M step
  pi1<-mysum(tau1)/length(dat)
  pi2<-mysum(tau2)/length(dat)
  
  mu1<-mysum(tau1*x)/mysum(tau1)
  mu2<-mysum(tau2*x)/mysum(tau2)
  
  sigma1<-mysum(tau1*(x-mu1)^2)/mysum(tau1)
  sigma2<-mysum(tau2*(x-mu2)^2)/mysum(tau2)
  
  #  loglik[k]<-sum(tau1*(log(pi1)+log(dnorm(x,mu1,sigma1))))+sum(tau2*(log(pi2)+log(dnorm(x,mu2,sigma2))))
  loglik[k+1]<-mysum(tau1*(log(pi1)+logdnorm(x,mu1,sigma1)))+mysum(tau2*(log(pi2)+logdnorm(x,mu2,sigma2)))
  k<-k+1
}

# compare
library(mixtools)
x = c(0,0,1,1,2,3,4,6,9)
gm<-normalmixEM(x,k=2,lambda=c(0.5,0.5),mu=c(-0.01,0.01),sigma=c(0.01,0.02))
gm$lambda
gm$mu
gm$sigma

gm$loglik