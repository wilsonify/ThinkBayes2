# Title     : An Introduction to R
# Objective : a simple Monte Carlo to explore the behavior of the two-sample t statistic
# Created by: thom
# Created on: 11/26/20

library(LearnBayes)

studentdata  <-  LearnBayes::studentdata

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

# Writing a Function to Compute the t Statistic
x <- rnorm(10,mean=50,sd=10)
y <- rnorm(10,mean=50,sd=10)
m <- length(x)
n <- length(y)
alpha=0.05
sp <- sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(m+n-2)) # pooled standard deviation
t.stat <- (mean(x)-mean(y))/(sp*sqrt(1/m+1/n))


tstatistic = function(x,y) {
  m=length(x)
  n=length(y)
  sp=sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(m+n-2))
  t.stat=(mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
  return(t.stat)
}

ttest = function(t,alpha,n,m) {
  tcrit <- qt(1-alpha/2, n+m-2)
  result <- abs(t.stat) > tcrit
  return(result)
}

data.x <- c(1,4,3,6,5)
data.y <- c(5,4,7,6,10)
tstatistic(data.x, data.y)

alpha=.1
m <- 10
n <- 10
N <- 10000 # sets the number of simulations
n.reject <- 0 # counter of number of rejections
for (i in 1:N) {
  #x=rnorm(m,mean=0,sd=1) # simulates xs from population 1
  #y=rnorm(n,mean=0,sd=1) # simulates ys from population 2
  
  #x=rnorm(m,mean=0,sd=1)
  #y=rnorm(n,mean=0,sd=1)
  
  #x=rnorm(m,mean=0,sd=1)
  #y=rnorm(n,mean=0,sd=10)
  
  #x=rt(m,df=4)
  #y=rt(n,df=4)
  
  #x=rexp(m,rate=1)
  #y=rexp(n,rate=1)
  
  #x=rnorm(m,mean=10,sd=2)
  #y=rexp(n,rate=1/10)
  
  x=rnorm(m,mean=10,sd=2)
  y=rexp(n,rate=1/10)
  
  statistic=tstatistic(x,y) # computes the t statistic
  tcrit <- qt(1-alpha/2, n+m-2)
  if (abs(statistic) > tcrit) { # reject if |T| exceeds critical pt
    n.reject=n.reject+1
  }
}
true.sig.level <- n.reject/N # proportion of rejections

my.tsimulation <- function() {
  alpha=.1
  m <- 10
  n <- 10
  x=rnorm(m,mean=10,sd=2)
  y=rexp(n,rate=1/10)
  statistic=tstatistic(x,y) # computes the t statistic
  return(statistic)
}

my.tsimulation()

tstat.vector <- replicate(10000, my.tsimulation())

plot(density(tstat.vector),xlim=c(-5,8),ylim=c(0,.4),lwd=3)

curve(dt(x,df=18),add=TRUE)

legend(4,.3,c("exact","t(18)"),lwd=c(3,1))







