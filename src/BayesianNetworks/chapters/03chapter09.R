# Title     : Regression Models
# Objective : illustrating the Bayesian fitting of a survival regression model.
# Created by: thom
# Created on: 11/26/20

data(birdextinct)
attach(birdextinct)
logtime=log(time)
plot(nesting,logtime)
out = (logtime > 3)
text(nesting[out], logtime[out], label=species[out], pos = 2)
plot(jitter(size),logtime,xaxp=c(0,1,1))
plot(jitter(status),logtime,xaxp=c(0,1,1))
fit=lm(logtime~nesting+size+status,data=birdextinct,x=TRUE, y=TRUE)
summary(fit)

theta.sample=blinreg(fit$y,fit$x,5000)

S=sum(fit$residual^2)
shape=fit$df.residual/2; rate=S/2
sigma2=rigamma(1,shape,rate)

MSE = sum(fit$residuals^2)/fit$df.residual
vbeta=vcov(fit)/MSE
beta=rmnorm(1,mean=fit$coef,varcov=vbeta*sigma2)


par(mfrow=c(2,2))
hist(theta.sample$beta[,2],main="NESTING",
     xlab=expression(beta[1]))
hist(theta.sample$beta[,3],main="SIZE",
     xlab=expression(beta[2]))
hist(theta.sample$beta[,4],main="STATUS",
     xlab=expression(beta[3]))
hist(theta.sample$sigma,main="ERROR SD",
     xlab=expression(sigma))

apply(theta.sample$beta,2,quantile,c(.05,.5,.95))

quantile(theta.sample$sigma,c(.05,.5,.95))

cov1=c(1,4,0,0)
cov2=c(1,4,1,0)
cov3=c(1,4,0,1)
cov4=c(1,4,1,1)
X1=rbind(cov1,cov2,cov3,cov4)
mean.draws=blinregexpected(X1,theta.sample)
c.labels=c("A","B","C","D")
par(mfrow=c(2,2))
for (j in 1:4) {
  hist(mean.draws[,j],
       main=paste("Covariate set",c.labels[j]),xlab="log TIME")
}


cov1=c(1,4,0,0)
cov2=c(1,4,1,0)
cov3=c(1,4,0,1)
cov4=c(1,4,1,1)
X1=rbind(cov1,cov2,cov3,cov4)
pred.draws=blinregpred(X1,theta.sample)
c.labels=c("A","B","C","D")
par(mfrow=c(2,2))
for (j in 1:4) {
  hist(pred.draws[,j],
       main=paste("Covariate set",c.labels[j]),xlab="log TIME")
}



pred.draws=blinregpred(fit$x,theta.sample)
pred.sum=apply(pred.draws,2,quantile,c(.05,.95))
par(mfrow=c(1,1))
ind=1:length(logtime)
matplot(rbind(ind,ind),pred.sum,type="l",lty=1,col=1,
        xlab="INDEX",ylab="log TIME")


points(ind,logtime,pch=19)
out=(logtime>pred.sum[2,])
text(ind[out], logtime[out], label=species[out], pos = 4)

prob.out=bayesresiduals(fit,theta.sample,2)
par(mfrow=c(1,1))
plot(nesting,prob.out)
out = (prob.out > 0.35)
text(nesting[out], prob.out[out], label=species[out], pos = 4)


data(puffin)
X=cbind(1, puffin$Distance - mean(puffin$Distance))
c.prior=c(0.1,0.5,5,2)
fit=vector("list",4)
for (j in 1:4)  {
  prior=list(b0=c(8,0), c0=c.prior[j])
  fit[[j]]=blinreg(puffin$Nest, X, 1000, prior)
}
BETA=NULL
for (j in 1:4) {
  s=data.frame(Prior=paste("c =",as.character(c.prior[j])),
               beta0=fit[[j]]$beta[,1],beta1=fit[[j]]$beta[,2])
  BETA=rbind(BETA,s)
}
library(lattice)
with(BETA,xyplot(beta1~beta0|Prior,type=c("p","g")))


data=list(y=puffin$Nest, X=cbind(1,puffin$Grass,puffin$Soil))
prior=list(b0=c(0,0,0), c0=100)
beta.start=with(puffin,lm(Nest~Grass+Soil)$coef)
laplace(reg.gprior.post,c(beta.start,0),
        list(data=data,prior=prior))$int


X=puffin[,-1]; y=puffin$Nest; c=100

bayes.model.selection(y,X,c,constant=FALSE)



# Survival Modeling

data(chemotherapy)
attach(chemotherapy)
library(survival)
survreg(Surv(time,status)~factor(treat)+age,dist="weibull")

weibullregpost=function(theta, data) {
  logf = function(t, c, x, sigma, mu, beta) {
    z = (log(t) - mu - x %*% beta)/sigma
    f = 1/sigma * exp(z - exp(z))
    S = exp(-exp(z))
    c * log(f) + (1 - c) * log(S) 
  }
  k = dim(data)[2]
  p = k - 2
  t = data[, 1]
  c = data[, 2]
  X = data[, 3:k]
  sigma = exp(theta[1])
  mu = theta[2]
  beta = array(theta[3:k], c(p, 1))
  return(sum(logf(t, c, X, sigma, mu, beta)))
}


start=c(-.5,9,.5,-.05)
d=cbind(time,status,treat-1,age)
fit=laplace(weibullregpost,start,d)
fit


proposal=list(var=fit$var,scale=1.5)

bayesfit=rwmetrop(weibullregpost,proposal,fit$mode,10000,d)

bayesfit$accept

par(mfrow=c(2,2))

sigma=exp(bayesfit$par[,1])

mu=bayesfit$par[,2]

beta1=bayesfit$par[,3]
beta2=bayesfit$par[,4]
hist(beta1,xlab="treatment")
hist(beta2,xlab="age",main="")
hist(sigma,xlab="sigma",main="")


