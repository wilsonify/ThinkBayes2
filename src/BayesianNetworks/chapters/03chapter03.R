# Title     : Single-Parameter Models
# Objective : TODO
# Created by: thom
# Created on: 11/26/20

data(footballscores)
attach(footballscores)
d = favorite - underdog - spread
n = length(d)
v = sum(d^2)


P = rchisq(1000, n)/v

s = sqrt(1/P)

hist(s,main="")

quantile(s, probs = c(0.025, 0.5, 0.975))









+



lambdaA = rgamma(1000, shape = alpha + yobs, rate = beta + ex)

ex = 1767; yobs=4

y = 0:10

py = dpois(y, lam * ex) * dgamma(lam, shape = alpha, rate = beta)/dgamma(lam, shape = alpha + y, rate = beta + ex)

cbind(y, round(py, 3))

lambdaB = rgamma(1000, shape = alpha + yobs, rate = beta + ex)































tscale = 20/qt(0.95, 2)

tscale







+ + + +







cbind(summ1,summ2)



















probs=c(.5,.5)

beta.par1=c(6, 14)

beta.par2=c(14, 6)









curve(post$probs[1]*dbeta(x,13,17)+post$probs[2]*dbeta(x,21,9),

curve(.5*dbeta(x,6,12)+.5*dbeta(x,12,6),0,1,add=TRUE)

legend("topleft",legend=c("Prior","Posterior"),lwd=c(1,3))

pbinom(5, 20, 0.5)













pbetat(p,.5,c(a,a),c(y,n-y))

prob.fair=function(log.a)

n = 20; y = 5; p = 0.5

curve(prob.fair(x), from=-4, to=5, xlab="log a",

















