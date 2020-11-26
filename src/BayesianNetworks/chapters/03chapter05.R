# Title     : Introduction to Bayesian Computation
# Objective : TODO
# Created by: thom
# Created on: 11/26/20

data(cancermortality)

mycontour(betabinexch0,c(.0001,.003,1,20000),cancermortality, xlab="eta",ylab="K")

mycontour(betabinexch,c(-8,-4.5,3,16.5),cancermortality, xlab="logit eta",ylab="log K")

fit=laplace(betabinexch,c(-7,6),cancermortality)

fit

npar=list(m=fit$mode,v=fit$var)

mycontour(lbinorm,c(-8,-4.5,3,16.5),npar, xlab="logit eta", ylab="log K")

se=sqrt(diag(fit$var))

fit$mode-1.645*se

fit$mode+1.645*se









tpar=list(m=fit$mode,var=2*fit$var,df=4)

datapar=list(data=cancermortality,par=tpar)

start=c(-6.9,12.4)

fit1=laplace(betabinT,start,datapar)

fit1$mode

betabinT(fit1$mode,datapar)

theta=rejectsampling(betabinexch,tpar,-569.2813,10000, cancermortality)

dim(theta)

mycontour(betabinexch,c(-8,-4.5,3,16.5),cancermortality,

points(theta[,1],theta[,2])









+







theta.s=sir(betabinexch,tpar,10000,cancermortality)

S=bayes.influence(theta.s,cancermortality)

plot(c(0,0,0),S$summary,type="b",lwd=3,xlim=c(-1,21),

for (i in 1:20)

