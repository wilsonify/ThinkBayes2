# Title     : Model Comparison
# Objective : TODO
# Created by: thom
# Created on: 11/26/20

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
























