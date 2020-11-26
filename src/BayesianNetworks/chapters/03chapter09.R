# Title     : Regression Models
# Objective : TODO
# Created by: thom
# Created on: 11/26/20


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










