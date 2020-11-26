# Title     : Hierarchical Modeling
# Objective : TODO
# Created by: thom
# Created on: 11/26/20

data(hearttransplants)

attach(hearttransplants)

plot(log(e), y/e, xlim=c(6,9.7), xlab="log(e)", ylab="y/e")

text(log(e),y/e,labels=as.character(y),pos=4)

sum(y)

sum(e)

lambda=rgamma(1000,shape=277,rate=294681)

ys94=rpois(1000,e[94]*lambda)

hist(ys94,breaks=seq(0.5,max(ys94)+0.5))

lines(c(y[94],y[94]),c(0,120),lwd=3)



+ + +



plot(log(e),pout,ylab="Prob(extreme)")

alpha=c(5,20,80,400); par(mfrow=c(2,2))

for (j in 1:4)









par(mfrow = c(1, 1))

mycontour(poissgamexch, c(0, 8, -7.3, -6.6), datapar, xlab="log alpha",ylab="log mu")

start = c(4, -7)

fitgibbs = gibbs(poissgamexch, start, 1000, c(1,.15), datapar)

fitgibbs$accept

mycontour(poissgamexch, c(0, 8, -7.3, -6.6), datapar, xlab="log alpha",ylab="log mu")

points(fitgibbs$par[, 1], fitgibbs$par[, 2])

plot(density(fitgibbs$par[, 1], bw = 0.2))

alpha = exp(fitgibbs$par[, 1])

mu = exp(fitgibbs$par[, 2])

lam1 = rgamma(1000, y[1] + alpha, e[1] + alpha/mu)







+ +

shrink=function(i) mean(alpha/(alpha + e[i] * mu))

shrinkage=sapply(1:94, shrink)

plot(log(e), shrinkage)

mrate=function(i) mean(rgamma(1000, y[i] + alpha, e[i]

hospital=1:94

meanrate=sapply(hospital,mrate)

hospital[meanrate==min(meanrate)]





+ + >

better[1:24,85]

log.alpha=fitgibbs$par[, 1]

log.alpha.new=sir.old.new(log.alpha, prior, prior.new)

lam94=rgamma(1000,y[94]+alpha,e[94]+alpha/mu)

ys94=rpois(1000,e[94]*lam94)

hist(ys94,breaks=seq(-0.5,max(ys94)+0.5))

lines(y[94]*c(1,1),c(0,100),lwd=3)

+ + + >

plot(pout,pout.exchange,xlab="P(extreme), equal means",

abline(0,1)











