# Title     : An Introduction to R
# Objective : TODO
# Created by: thom
# Created on: 11/26/20

studentdata  <-  read.table("studentdata.txt", sep = "\t", header = TRUE)

data(studentdata)

studentdata[1, ]

attach(studentdata)

table(Drink)

table(Drink)

barplot(table(Drink),xlab="Drink",ylab="Count")

hours.of.sleep <-  WakeUp - ToSleep

summary(hours.of.sleep)

hist(hours.of.sleep,main="")

boxplot(hours.of.sleep~Gender, ylab="Hours of Sleep")

female.Haircut <- Haircut[Gender=="female"]

male.Haircut <- Haircut[Gender=="male"]

summary(female.Haircut)

summary(male.Haircut)

plot(jitter(ToSleep),jitter(hours.of.sleep))

fit <- lm(hours.of.sleep~ToSleep)

fit

abline(fit)

x <- rnorm(10,mean=50,sd=10)

y <- rnorm(10,mean=50,sd=10)

m <- length(x)

n <- length(y)

sp <- sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(m+n-2))

t.stat <- (mean(x)-mean(y))/(sp*sqrt(1/m+1/n))

source("tstatistic.R")

data.x <- c(1,4,3,6,5)

data.y <- c(5,4,7,6,10)

tstatistic(data.x, data.y)

m <- 10; n <- 10

my.tsimulation <- function()

tstat.vector <- replicate(10000, my.tsimulation())

plot(density(tstat.vector),xlim=c(-5,8),ylim=c(0,.4),lwd=3)

curve(dt(x,df=18),add=TRUE)

legend(4,.3,c("exact","t(18)"),lwd=c(3,1))









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









+ > >



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











weights=c(182, 172, 173, 176, 176, 180, 173, 174, 179, 175)

ybar=mean(weights)

sigma2=3^2/length(weights)

post.precision=1/sigma2+1/pvar

post.var=1/post.precision

post.mean=(ybar/sigma2+pmean/pvar)/post.precision

c(post.mean,sqrt(post.var))

post.odds=pnorm(175,post.mean,sqrt(post.var))/

post.odds

BF = post.odds/prior.odds

BF

postH=probH*BF/(probH*BF+probA)

postH

z=sqrt(length(weights))*(mean(weights)-175)/3

1-pnorm(z)













































s=laplace(bfexch,0,list(data=data,K=K0))









log.marg=function(logK) laplace(bfexch,0,list(data=data1,K=exp(logK)))$int

log.K=seq(2,6)

K=exp(log.K)

log.BF=sapply(log.K,log.marg)

BF=exp(log.BF)

round(data.frame(log.K,K,log.BF,BF),2)

12

data=matrix(c(11,9,68,23,3,5),c(2,3))

data

chisq.test(data)

12

12

a=matrix(rep(1,6),c(2,3))

a

ctable(data,a)

12

























fit=lm(logtime~nesting+size+status,data=birdextinct,x=TRUE,

summary(fit)

theta.sample=blinreg(fit$y,fit$x,5000)



> >

hist(theta.sample$sigma,main="ERROR SD",

apply(theta.sample$beta,2,quantile,c(.05,.5,.95))

quantile(theta.sample$sigma,c(.05,.5,.95))



















































points(ind,logtime,pch=19)

out=(logtime>pred.sum[2,])

text(ind[out], logtime[out], label=species[out], pos = 4)





















+ + >











X=puffin[,-1]; y=puffin$Nest; c=100

bayes.model.selection(y,X,c,constant=FALSE)

















proposal=list(var=fit$var,scale=1.5)

bayesfit=rwmetrop(weibullregpost,proposal,fit$mode,10000,d)

bayesfit$accept

par(mfrow=c(2,2))

sigma=exp(bayesfit$par[,1])

mu=bayesfit$par[,2]











data(darwin)

attach(darwin)

fit=robustt(difference,4,10000)

plot(density(fit$mu),xlab="mu")











data(donner)

attach(donner)

X=cbind(1,age,male)

fit=glm(survival~X-1,family=binomial(link=probit))

summary(fit)

m=10000

fit=bayes.probit(survival,X,m)

apply(fit$beta,2,mean)

apply(fit$beta,2,sd)

a=seq(15,65)

X1=cbind(1,a,1)

p.male=bprobit.probs(X1,fit$beta)

plot(a,apply(p.male,2,quantile,.5),type="l",ylim=c(0,1), xlab="age",ylab="Probability of Survival")

lines(a,apply(p.male,2,quantile,.05),lty=2)

lines(a,apply(p.male,2,quantile,.95),lty=2)











bayes.probit(y,X,1000,list(beta=beta0,P=P0))$log.marg

bayes.probit(y,X[,-2],1000, list(beta=beta0[-2],P=P0[-2,-2]))$log.marg

bayes.probit(y,X[,-3],1000, list(beta=beta0[-3],P=P0[-3,-3]))$log.marg

bayes.probit(y,X[,-c(2,3)],1000, list(beta=beta0[-c(2,3)],P=P0[-c(2,3),-c(2,3)]))$log.marg

data(iowagpa)

rlabels = c("91-99", "81-90", "71-80", "61-70", "51-60", "41-50","31-40", "21-30")

clabels = c("16-18", "19-21", "22-24", "25-27", "28-30")

gpa = matrix(iowagpa[, 1], nrow = 8, ncol = 5, byrow = T)

dimnames(gpa) = list(HSR = rlabels, ACTC = clabels)

gpa

samplesizes = matrix(iowagpa[, 2], nrow = 8, ncol = 5,

dimnames(samplesizes) = list(HSR = rlabels, ACTC = clabels)

samplesizes

act = seq(17, 29, by = 3)

matplot(act, t(gpa), type = "l", lwd = 2,

legend(30, 3, lty = 1:8, lwd = 2, legend = c("HSR=9", "HSR=8", "HSR=7", "HSR=6", "HSR=5", "HSR=4", "HSR=3", "HSR=2"))

MU = ordergibbs(iowagpa, 5000)











matplot(act, t(postmeans), type = "l", lwd = 2,

legend(30, 3, lty = 1:8, lwd = 2, legend = c("HSR=9", "HSR=8", "HSR=7", "HSR=6", "HSR=5", "HSR=4", "HSR=3", "HSR=2"))











s=.65

se=s/sqrt(samplesizes)

round(postsds/se,2)

FIT=hiergibbs(iowagpa,5000)



> >

quantile(FIT$beta[,3],c(.025,.25,.5,.75,.975))

quantile(FIT$var,c(.025,.25,.5,.75,.975))

posterior.means = apply(FIT$mu, 2, mean)

posterior.means = matrix(posterior.means, nrow = 8, ncol = 5,

par(mfrow=c(1,1))

matplot(act, t(posterior.means), type = "l", lwd = 2, xlim = c(17, 34))

legend(30, 3, lty = 1:8, lwd = 2, legend = c("HSR=9", "HSR=8", "HSR=7", "HSR=6", "HSR=5", "HSR=4", "HSR=3", "HSR=2"))

p=1-pnorm((2.5-FIT$mu)/.65)

prob.success=apply(p,2,mean)

prob.success=matrix(prob.success,nrow=8,ncol=5,byrow=T)

dimnames(prob.success)=list(HSR=rlabels,ACTC=clabels)

round(prob.success,3)

model.sim <- bugs (data, inits, parameters, "model.bug")

N=112

D=c(4,5,4,1,0,4,3,4,0,6,

data=list("N","D")

parameters <- c("changeyear","b")

inits = function() {list(b=c(0,0),changeyear=50)}

coalmining.sim <- bugs (data, inits, parameters, "coalmining.bug", n.chains=3, n.iter=1000, codaPkg=TRUE)

coalmining.coda = read.bugs(coalmining.sim)

summary(coalmining.coda)

xyplot(coalmining.coda)

acfplot(coalmining.coda)

densityplot(coalmining.coda,col="black")











inits = function() {list(b=c(0,0),tau=1)}

data=list("N","y","x")

inits = function() {list(b=c(0,0),tau=1)}

parameters <- c("tau","lam","b")

robust.sim <- bugs (data, inits, parameters, "robust.bug")



















data(sluggerdata)

s=careertraj.setup(sluggerdata)

N=s$N; T=s$T; y=s$y; n=s$n; x=s$x

peak.age=matrix(0,50000,10)

for (i in 1:10)

peak.age[,i]=-career.sim$sims.list$beta[,i,2]/2/ career.sim$sims.list$beta[,i,3]

dimnames(peak.age)[[2]]=c("Aaron","Greenberg", "Killebrew", "Mantle","Mays", "McCovey" ,"Ott", "Ruth", "Schmidt", "Sosa")

densityplot(as.mcmc(peak.age),plot.points=FALSE)

summary(as.mcmc(peak.age))

657

Process finished with exit code 0
