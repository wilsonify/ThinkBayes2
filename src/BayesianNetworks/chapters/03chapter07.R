# Title     : Hierarchical Modeling
# Objective : simulate hyperparameters, with an exchangeable hierarchical model. 
# Created by: thom
# Created on: 11/26/20

data(hearttransplants)
attach(hearttransplants)

plot(log(e), y/e, xlim=c(6,9.7), xlab="log(e)", ylab="y/e")
text(log(e),y/e,labels=as.character(y),pos=4)

sum(y)
sum(e)

lambda <- rgamma(1000,shape=277,rate=294681)
ys94 <- rpois(1000,e[94]*lambda)
hist(ys94,breaks=seq(0.5,max(ys94)+0.5))
lines(c(y[94],y[94]),c(0,120),lwd=3)

lambda <- rgamma(1000,shape=277,rate=294681)
prob.out <- function(i) {
  ysi <- rpois(1000,e[i]*lambda)
  pleft <- sum(ysi<=y[i])/1000
  pright <- sum(ysi>=y[i])/1000
  min(pleft,pright)
}
pout <- sapply(1:94,prob.out)
plot(log(e),pout,ylab="Prob(extreme)")

pgexchprior <- function(lambda,pars) {
  alpha <- pars[1]; a <- pars[2]; b <- pars[3]
  (alpha-1)*log(prod(lambda))-(2*alpha+a)*log(alpha*sum(lambda)+b)
}


alpha <- c(5,20,80,400)
par(mfrow=c(2,2))
for (j in 1:4) {
  mycontour(
    pgexchprior,
    c(.001,5,.001,5),
    c(alpha[j],10,10), 
    main=paste("ALPHA = ",alpha[j]),
    xlab="LAMBDA 1",
    ylab="LAMBDA 2"
    )
}


poissgamexch <- function (theta, datapar) {
  y  <-  datapar$data[, 2]
  e  <-  datapar$data[, 1]
  z0  <-  datapar$z0
  alpha  <-  exp(theta[1])
  mu  <-  exp(theta[2])
  beta  <-  alpha/mu
  logf  <-  function(y, e, alpha, beta) {
    lgamma(alpha + y) - (y + alpha) * log(e + beta) + alpha * log(beta) - lgamma(alpha)
  }
  val  <-  sum(logf(y, e, alpha, beta))
  val  <-  val + log(alpha) - 2 * log(alpha + z0)
  return(val)
}



datapar  <-  list(data = hearttransplants, z0 = 0.53)
start <- c(2, -7)
fit  <-  laplace(poissgamexch, start, datapar)
fit


par(mfrow = c(1, 1))
mycontour(poissgamexch, c(0, 8, -7.3, -6.6), datapar, xlab="log alpha",ylab="log mu")


start  <-  c(4, -7)
fitgibbs  <-  gibbs(poissgamexch, start, 1000, c(1,.15), datapar)
fitgibbs$accept

mycontour(poissgamexch, c(0, 8, -7.3, -6.6), datapar, xlab="log alpha",ylab="log mu")

points(fitgibbs$par[, 1], fitgibbs$par[, 2])

plot(density(fitgibbs$par[, 1], bw = 0.2))

alpha  <-  exp(fitgibbs$par[, 1])

mu  <-  exp(fitgibbs$par[, 2])

lam1  <-  rgamma(1000, y[1] + alpha, e[1] + alpha/mu)


alpha  <-  exp(fitgibbs$par[, 1])
mu  <-  exp(fitgibbs$par[, 2])
plot(log(e), y/e, pch = as.character(y))
for (i in 1:94) {
  lami  <-  rgamma(1000, y[i] + alpha, e[i] + alpha/mu)
  probint  <-  quantile(lami, c(0.05, 0.95))
  lines(log(e[i]) * c(1, 1), probint)
}



shrink <- function(i) mean(alpha/(alpha + e[i] * mu))

shrinkage <- sapply(1:94, shrink)

plot(log(e), shrinkage)

mrate <- function(i) mean(rgamma(1000, y[i] + alpha, e[i] + alpha/mu))
hospital <- 1:94
meanrate <- sapply(hospital,mrate)
hospital[meanrate==min(meanrate)]

sim.lambda <- function(i) rgamma(1000,y[i]+alpha,e[i]+alpha/mu)
LAM <- sapply(1:94,sim.lambda)
compare.rates <- function(x) {
  nc <- NCOL(x)
  ij <- as.matrix(expand.grid(1:nc, 1:nc))
  m <- as.matrix(x[,ij[,1]] > x[,ij[,2]])
  matrix(colMeans(m), nc, nc, byrow = TRUE)
}
better <- compare.rates(LAM)

better[1:24,85]


sir.old.new <- function(theta, prior, prior.new) {
  log.g <- log(prior(theta))
  log.g.new <- log(prior.new(theta))
  wt <- exp(log.g.new-log.g-max(log.g.new-log.g))
  probs <- wt/sum(wt)
  n <- length(probs)
  indices <- sample(1:n,size=n,prob=probs,replace=TRUE)
  theta[indices]
}

prior <- function(theta) 0.53*exp(theta)/(exp(theta)+0.53)^2
prior.new <- function(theta) 5*exp(theta)/(exp(theta)+5)^2
log.alpha <- fitgibbs$par[, 1]

log.alpha.new <- sir.old.new(log.alpha, prior, prior.new)

lam94 <- rgamma(1000,y[94]+alpha,e[94]+alpha/mu)

ys94 <- rpois(1000,e[94]*lam94)

hist(ys94,breaks=seq(-0.5,max(ys94)+0.5))

lines(y[94]*c(1,1),c(0,100),lwd=3)

prob.out <- function(i) {
  lami <- rgamma(1000,y[i]+alpha,e[i]+alpha/mu)
  ysi <- rpois(1000,e[i]*lami)
  pleft <- sum(ysi<=y[i])/1000
  pright <- sum(ysi>=y[i])/1000
  min(pleft,pright)
}
pout.exchange <- sapply(1:94,prob.out)

plot(pout,pout.exchange,xlab="P(extreme), equal means", ylab="P(extreme), exchangeable")
abline(0,1)
