# Title     : Markov Chain Monte Carlo Methods
# Objective : TODO
# Created by: thom
# Created on: 11/26/20

library(coda)

P=matrix(
  c(.5,.5,0,0,0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,0,0,.5,.5),
  nrow=6,ncol=6,byrow=TRUE
  )
P

s=array(0,c(50000,1))

s[1]=3

for (j in 2:50000) s[j]=sample(1:6,size=1,prob=P[s[j-1],])

m=c(500,2000,8000,50000)

for (i in 1:4) print(table(s[1:m[i]])/m[i])

w=matrix(c(.1,.2,.2,.2,.2,.1),nrow=1,ncol=6)

w%*%P


groupeddatapost=function(theta,data) {
  dj = function(f, int.lo, int.hi, mu, sigma) {
    f * log(pnorm(int.hi, mu, sigma) - pnorm(int.lo, mu, sigma))
  }
  mu = theta[1]
  sigma = exp(theta[2])
  sum(dj(data$f, data$int.lo, data$int.hi, mu, sigma))
}

d=list(
  int.lo=c(-Inf,seq(66,74,by=2)),
  int.hi=c(seq(66,74,by=2), Inf),
  f=c(14,30,49,70,33,15)
  )

y=c(
  rep(65,14),
  rep(67,30),
  rep(69,49),
  rep(71,70),
  rep(73,33),
  rep(75,15)
  )
mean(y)
log(sd(y))


start=c(70,1)
fit=laplace(groupeddatapost,start,d)
fit

modal.sds=sqrt(diag(fit$var))

proposal=list(var=fit$var,scale=2)

fit2=rwmetrop(groupeddatapost,proposal,start,10000,d)

fit2$accept

post.means=apply(fit2$par,2,mean)
post.sds=apply(fit2$par,2,sd)

cbind(c(fit$mode),modal.sds)

cbind(post.means,post.sds)

mycontour(groupeddatapost,c(69,71,.6,1.3),d, xlab="mu",ylab="log sigma")

points(fit2$par[5001:10000,1],fit2$par[5001:10000,2])

start=c(65,1)
proposal=list(var=fit$var,scale=0.2)
bayesfit=rwmetrop(groupeddatapost,proposal,start,10000,d)

dimnames(bayesfit$par)[[2]]=c("mu","log sigma")

xyplot(mcmc(bayesfit$par[-c(1:2000),]),col="black")

par(mfrow=c(2,1))

autocorr.plot(mcmc(bayesfit$par[-c(1:2000),]),auto.layout=FALSE)


cauchyerrorpost=function (theta, data) {
  logf = function(data, theta) {
    log(dt((data - theta[1])/exp(theta[2]), df = 1)/exp(theta[2]))
  }
  return(sum(logf(data, theta)))
}

data(darwin)
attach(darwin)
mean(difference)
log(sd(difference))

laplace(cauchyerrorpost, c(21.6,3.6), difference)

c(24.7-4*sqrt(34.96),24.7+4*sqrt(34.96))

c(2.77-4*sqrt(.138),2.77+4*sqrt(.138))
mycontour(cauchyerrorpost,c(-10,60,1,4.5),difference, xlab="mu",ylab="log sigma")


fitlaplace=laplace(cauchyerrorpost,c(21.6,3.6), difference)
mycontour(
  lbinorm,
  c(-10,60,1,4.5),
  list(m=fitlaplace$mode, v=fitlaplace$var),
  xlab="mu",
  ylab="log sigma"
  )


proposal=list(var=fitlaplace$var,scale=2.5)
start=c(20,3)
m=1000
s=rwmetrop(cauchyerrorpost,proposal,start,m,difference)
mycontour(cauchyerrorpost,c(-10,60,1,4.5),difference, xlab="mu",ylab="log sigma")
points(s$par[,1],s$par[,2])


fitgrid=simcontour(cauchyerrorpost,c(-10,60,1,4.5),difference, 50000)
proposal=list(var=fitlaplace$var,scale=2.5)
start=c(20,3)
fitrw=rwmetrop(cauchyerrorpost,proposal,start,50000, difference)
proposal2=list(var=fitlaplace$var,mu=t(fitlaplace$mode))
fitindep=indepmetrop(cauchyerrorpost,proposal2,start,50000, difference)
fitgibbs=gibbs(cauchyerrorpost,start,50000,c(12,.75), difference)

apply(fitrw$par,2,mean)

apply(fitrw$par,2,sd)


data(stanfordheart)
attach(stanfordheart)

transplantpost=function (theta, data) {
  x = data[, 1]
  y = data[, 3]
  t = data[, 2]
  d = data[, 4]
  tau = exp(theta[1])
  lambda = exp(theta[2])
  p = exp(theta[3])
  xnt = x[t == 0]
  dnt = d[t == 0]
  z = x[t == 1]
  y = y[t == 1]
  dt = d[t == 1]
  logf = function(xnt, dnt, lambda, p) {
    (dnt == 0) * (p * log(lambda) + log(p) - (p + 1) * log(lambda + xnt)) + (dnt == 1) * p * log(lambda/(lambda + xnt))
  }
  logg = function(z, y, tau, lambda, p) {
    (dt == 0) * (p * log(lambda) + log(p * tau) - (p + 1) * log(lambda + y + tau * z)) + (dt == 1) * p * log(lambda/(lambda + y + tau * z))
  }
  val = sum(logf(xnt, dnt, lambda, p)) + sum(logg(z, y, tau, lambda, p)) 
  val = val + theta[1] + theta[2] + theta[3]
  return(val)
}

start=c(0,3,-1)
laplacefit=laplace(transplantpost,start,stanfordheart)
laplacefit

proposal=list(var=laplacefit$var,scale=2)
s=rwmetrop(transplantpost,proposal,start,10000,stanfordheart)
s$accept

tau=exp(s$par[,1])
lambda=exp(s$par[,2])
p=exp(s$par[,3])


plot(density(tau),main="TAU")
plot(density(lambda),main="LAMBDA")
plot(density(p),main="P")

apply(exp(s$par),2,quantile,c(.05,.5,.95))



t=seq(1,240)
p5=0*t; p50=0*t; p95=0*t
for (j in 1:240) {
  S=(lambda/(lambda+t[j]))^p
  q=quantile(S,c(.05,.5,.95))
  p5[j]=q[1]; p50[j]=q[2]; p95[j]=q[3]
}
plot(t,p50,type="l",ylim=c(0,1),ylab="Prob(Survival)",
     xlab="time")
lines(t,p5,lty=2)
lines(t,p95,lty=2)


