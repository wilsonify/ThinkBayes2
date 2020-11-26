# Title     : Markov Chain Monte Carlo Methods
# Objective : TODO
# Created by: thom
# Created on: 11/26/20

P=matrix(c(.5,.5,0,0,0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0, 0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,0,0,.5,.5),

P

s=array(0,c(50000,1))

s[1]=3

for (j in 2:50000) s[j]=sample(1:6,size=1,prob=P[s[j-1],])

m=c(500,2000,8000,50000)

for (i in 1:4) print(table(s[1:m[i]])/m[i])

w=matrix(c(.1,.2,.2,.2,.2,.1),nrow=1,ncol=6)

w%*%P

+

y=c(rep(65,14),rep(67,30),rep(69,49),rep(71,70),rep(73,33),

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

library(coda)

dimnames(bayesfit$par)[[2]]=c("mu","log sigma")

xyplot(mcmc(bayesfit$par[-c(1:2000),]),col="black")

par(mfrow=c(2,1))

autocorr.plot(mcmc(bayesfit$par[-c(1:2000),]),auto.layout=FALSE)

data(darwin)

attach(darwin)

mean(difference)

log(sd(difference))

laplace(cauchyerrorpost, c(21.6,3.6), difference)

c(24.7-4*sqrt(34.96),24.7+4*sqrt(34.96))

c(2.77-4*sqrt(.138),2.77+4*sqrt(.138))



fitlaplace=laplace(cauchyerrorpost,c(21.6,3.6), difference)

mycontour(lbinorm,c(-10,60,1,4.5),list(m=fitlaplace$mode, v=fitlaplace$var)), xlab="mu",ylab="log sigma")

















fitindep=indepmetrop(cauchyerrorpost,proposal2,start,50000,

fitgibbs=gibbs(cauchyerrorpost,start,50000,c(12,.75),

apply(fitrw$par,2,mean)

apply(fitrw$par,2,sd)

data(stanfordheart)

start=c(0,3,-1)

laplacefit=laplace(transplantpost,start,stanfordheart)

laplacefit

proposal=list(var=laplacefit$var,scale=2)

s=rwmetrop(transplantpost,proposal,start,10000,stanfordheart)

s$accept

tau=exp(s$par[,1])

plot(density(tau),main="TAU")

apply(exp(s$par),2,quantile,c(.05,.5,.95))









