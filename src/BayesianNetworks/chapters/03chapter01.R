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







