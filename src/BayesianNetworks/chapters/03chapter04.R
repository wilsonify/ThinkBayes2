# Title     : Multiparameter Models
# Objective : TODO
# Created by: thom
# Created on: 11/26/20
library(LearnBayes)

data(marathontimes)
attach(marathontimes)
d = mycontour(normchi2post, c(220, 330, 500, 9000), time, xlab="mean",ylab="variance")


S = sum((time - mean(time))^2)
n = length(time)
sigma2 = S/rchisq(1000, n - 1)
mu = rnorm(1000, mean = mean(time), sd = sqrt(sigma2)/sqrt(n))

points(mu, sigma2)

quantile(mu, c(0.025, 0.975))

quantile(sqrt(sigma2), c(0.025, 0.975))

alpha = c(728, 584, 138)

theta = rdirichlet(1000, alpha)

hist(theta[, 1] - theta[, 2], main="")



election.2008 <- LearnBayes::election.2008

prob.Obama=function(j) {
  p=rdirichlet(
    5000, 
    500*c(
      election.2008$M.pct[j],
      election.2008$O.pct[j],
      100-election.2008$M.pct[j]-election.2008$O.pct[j])/100+1
    )
  mean(p[,2]>p[,1])
}

Obama.win.probs=sapply(1:51,prob.Obama)

sim.election=function() {
  winner=rbinom(51,1,Obama.win.probs)
  sum(election.2008$EV*winner)
}

sim.EV <- replicate(1000,sim.election())

hist(sim.EV,min(sim.EV):max(sim.EV),col="blue")

abline(v=365,lwd=3) # Obama received 365 votes

text(375,30,"Actual \n Obama \n total")


# A Bioassay Experiment

dose = c(-0.86, -0.3, -0.05, 0.73)
sample_size = c(5, 5, 5, 5)
deaths = c(0, 1, 3, 5)
beta=c(2,10)
data = cbind(dose, sample_size, deaths)
logisticpost(beta,data)


response = cbind(deaths, sample_size - deaths)
results = glm(response ~ dose, family = binomial)
summary(results)


beta.select(list(p=.5,x=.2),list(p=.9,x=.5))
beta.select(list(p=.5,x=.8),list(p=.9,x=.98))

prior=rbind(
  c(-0.7, 4.68, 1.12),
  c(0.6, 2.10, 0.74)
  )

data.new=rbind(data, prior)



#plot for figure 4.4?

mycontour(logisticpost,c(-3,3,-1,9),data.new, xlab="beta0", ylab="beta1")

s=simcontour(logisticpost,c(-2,3,-1,11),data.new,1000)

points(s)

plot(density(s$y),xlab="beta1")

theta=-s$x/s$y

hist(theta,xlab="LD-50",breaks=20)

quantile(theta,c(.025,.975))

sigma=c(2,1,.5,.25)
plo=.0001;phi=.9999
par(mfrow=c(2,2))
for (i in 1:4) {
  mycontour(
    howardprior,
    c(plo,phi,plo,phi),
    c(1,1,1,1,sigma[i]), 
    main=paste("sigma=",as.character(sigma[i])), 
    xlab="p1",
    ylab="p2"
    )
}


sigma=c(2,1,.5,.25)
par(mfrow=c(2,2))
for (i in 1:4) {
  mycontour(
    howardprior,
    c(plo,phi,plo,phi),
    c(1+3,1+15,1+7,1+5,sigma[i]),
    main=paste("sigma=",as.character(sigma[i])),
    xlab="p1",
    ylab="p2")
  lines(c(0,1),c(0,1))
}

s=simcontour(howardprior,c(plo,phi,plo,phi), c(1+3,1+15,1+7,1+5,2),1000)

sum(s$x>s$y)/1000

