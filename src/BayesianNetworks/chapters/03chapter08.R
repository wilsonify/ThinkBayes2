# Title     : Model Comparison
# Objective : illustrate Bayes factor computations
# Created by: thom
# Created on: 11/26/20


pmean=170; pvar=25
probH=pnorm(175,pmean,sqrt(pvar))
probA=1-probH
prior.odds=probH/probA
prior.odds

weights=c(182, 172, 173, 176, 176, 180, 173, 174, 179, 175)
ybar=mean(weights)
sigma2=3^2/length(weights)

post.precision=1/sigma2+1/pvar
post.var=1/post.precision

post.mean=(ybar/sigma2+pmean/pvar)/post.precision

c(post.mean,sqrt(post.var))

post.odds=pnorm(175,post.mean,sqrt(post.var))/ (1-pnorm(175,post.mean,sqrt(post.var)))
post.odds

BF = post.odds/prior.odds
BF

postH=probH*BF/(probH*BF+probA)
postH

z=sqrt(length(weights))*(mean(weights)-175)/3
1-pnorm(z)

weights=c(182, 172, 173, 176, 176, 180, 173, 174, 179, 175)
data=c(mean(weights),length(weights),3)
prior.par=c(170,1000)
mnormt.onesided(175,prior.par,data)

weights=c(182, 172, 173, 176, 176, 180, 173, 174, 179, 175)
data=c(mean(weights),length(weights),3)
t=c(.5,1,2,4,8)
mnormt.twosided(170,.5,t,data)


logpoissgamma=function(theta,datapar) {
  y=datapar$data
  npar=datapar$par
  lambda=exp(theta)
  loglike=log(dgamma(lambda,shape=sum(y)+1,rate=length(y)))
  logprior=log(dgamma(lambda,shape=npar[1],rate=npar[2])*lambda)
  return(loglike+logprior)
}

logpoissnormal=function(theta,datapar) {
  y=datapar$data
  npar=datapar$par
  lambda=exp(theta)
  loglike=log(dgamma(lambda,shape=sum(y)+1,scale=1/length(y)))
  logprior=log(dnorm(theta,mean=npar[1],sd=npar[2]))
  return(loglike+logprior)
}

data(soccergoals)
attach(soccergoals)
datapar=list(data=goals,par=c(4.57,1.43))
fit1=laplace(logpoissgamma,.5,datapar)
datapar=list(data=goals,par=c(1,.5))
fit2=laplace(logpoissnormal,.5,datapar)
datapar=list(data=goals,par=c(2,.5))
fit3=laplace(logpoissnormal,.5,datapar)



datapar=list(data=goals,par=c(1,2))
fit4=laplace(logpoissnormal,.5,datapar)


postmode=c(fit1$mode,fit2$mode,fit3$mode,fit4$mode)
postsd=sqrt(c(fit1$var,fit2$var,fit3$var,fit4$var))
logmarg=c(fit1$int,fit2$int,fit3$int,fit4$int)
cbind(postmode,postsd,logmarg)


bfexch=function (theta, datapar) {
  y = datapar$data[, 1]
  n = datapar$data[, 2]
  K = datapar$K
  eta = exp(theta)/(1 + exp(theta))
  logf = function(K, eta, y, n)
    lbeta(K * eta + y, K * (1 - eta) + n - y) -
    lbeta(K * eta, K * (1 - eta))
  sum(logf(K, eta, y, n)) + log(eta * (1 - eta)) -
    lbeta(sum(y) + 1, sum(n - y) + 1)
}


# s=laplace(bfexch,0,list(data=data,K=K0))


data(jeter2004)
attach(jeter2004)
data=cbind(H,AB)
data1=regroup(data,5)

log.marg=function(logK) laplace(bfexch,0,list(data=data1,K=exp(logK)))$int

log.K=seq(2,6)

K=exp(log.K)

log.BF=sapply(log.K,log.marg)

BF=exp(log.BF)

round(data.frame(log.K,K,log.BF,BF),2)



data=matrix(c(11,9,68,23,3,5),c(2,3))

data

chisq.test(data)

a=matrix(rep(1,6),c(2,3))

a

ctable(data,a)

log.K=seq(2,7)
compute.log.BF=function(log.K) log(bfindep(data,exp(log.K),100000)$bf)
log.BF=sapply(log.K,compute.log.BF)
BF=exp(log.BF)
round(data.frame(log.K,log.BF,BF),2)


