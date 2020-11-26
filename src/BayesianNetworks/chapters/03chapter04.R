# Title     : Multiparameter Models
# Objective : TODO
# Created by: thom
# Created on: 11/26/20

data(marathontimes)

attach(marathontimes)

d = mycontour(normchi2post, c(220, 330, 500, 9000), time, xlab="mean",ylab="variance")









points(mu, sigma2)

quantile(mu, c(0.025, 0.975))

quantile(sqrt(sigma2), c(0.025, 0.975))

alpha = c(728, 584, 138)

theta = rdirichlet(1000, alpha)

hist(theta[, 1] - theta[, 2], main="")

library(LearnBayes)

data(election.2008)

attach(data)

+ +

Obama.win.probs=sapply(1:51,prob.Obama)

+ +



hist(sim.EV,min(sim.EV):max(sim.EV),col="blue")

abline(v=365,lwd=3) # Obama received 365 votes

text(375,30,"Actual \n Obama \n total")









response = cbind(y, n - y)

results = glm(response ~ x, family = binomial)

summary(results)

beta.select(list(p=.5,x=.2),list(p=.9,x=.5))

beta.select(list(p=.5,x=.8),list(p=.9,x=.98))

prior=rbind(c(-0.7, 4.68, 1.12), c(0.6, 2.10, 0.74))

data.new=rbind(data, prior)

mycontour(logisticpost,c(-3,3,-1,9),data.new,

s=simcontour(logisticpost,c(-2,3,-1,11),data.new,1000)

points(s)

plot(density(s$y),xlab="beta1")

theta=-s$x/s$y

hist(theta,xlab="LD-50",breaks=20)

quantile(theta,c(.025,.975))









+

sigma=c(2,1,.5,.25)

par(mfrow=c(2,2))

for (i in 1:4)

s=simcontour(howardprior,c(plo,phi,plo,phi), c(1+3,1+15,1+7,1+5,2),1000)

sum(s$x>s$y)/1000

