# Title     : Gibbs Sampling
# Objective : TODO
# Created by: thom
# Created on: 11/26/20


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

