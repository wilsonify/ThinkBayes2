# Title     : Introduction to Bayesian Thinking
# Objective : TODO
# Created by: thom
# Created on: 11/26/20


p = seq(0.05, 0.95, by = 0.1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior = prior/sum(prior)
plot(p, prior, type = "h", ylab="Prior Probability")

data <- c(11, 16)

post <- pdisc(p, prior, data)

round(cbind(p, prior, post),2)















quantile2 <- list(p=.9,x=.5)

quantile1 <- list(p=.5,x=.3)

beta.select(quantile1,quantile2)















1 - pbeta(0.5, a + s, b + f)

qbeta(c(0.05, 0.95), a + s, b + f)

ps = rbeta(1000, a + s, b + f)

hist(ps,xlab="p",main="")

sum(ps >= 0.5)/1000

quantile(ps, c(0.05, 0.95))

midpt = seq(0.05, 0.95, by = 0.1)

prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)

prior = prior/sum(prior)

curve(histprior(x,midpt,prior), from=0, to=1, ylab="Prior density",ylim=c(0,.3))

curve(histprior(x,midpt,prior) * dbeta(x,s+1,f+1), from=0, to=1, ylab="Posterior density")

p = seq(0, 1, length=500)

post = histprior(p, midpt, prior) * dbeta(p, s+1, f+1)

post = post/sum(post)

ps = sample(p, replace = TRUE, prob = post)

hist(ps, xlab="p", main="")













ab=c(3.26, 7.19)

m=20; ys=0:20

pred=pbetap(ab, m, ys)

p=rbeta(1000, 3.26, 7.19)

y = rbinom(1000, 20, p)

table(y)









dist=cbind(ys,predprob)

dist

covprob=.9

discint(dist,covprob)









