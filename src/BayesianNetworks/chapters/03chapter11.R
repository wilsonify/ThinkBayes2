

model <- function() {
  y ~ dbin(p, n)
  p ~ dbeta(alpha, beta)
}

data <- list(y = 7, n = 50, alpha = 0.5, beta = 0.5)

inits <- list(p = 0.1)

# model.sim <- bugs (data, inits, parameters, "model.bug")

N=112
D=c(4,5,4,1,0,4,3,4,0,6, 
    3,3,4,0,2,6,3,3,5,4,5,3,1,4,4,1,5,5,3,4,2,5,2,2,3,4,2,1,3,2,
    1,1,1,1,1,3,0,0,1,0,1,1,0,0,3,1,0,3,2,2,
    0,1,1,1,0,1,0,1,0,0,0,2,1,0,0,0,1,1,0,2,
    2,3,1,1,2,1,1,1,1,2,4,2,0,0,0,1,4,0,0,0,
    1,0,0,0,0,0,1,0,0,1,0,0)
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

for (i in 1:10) {
  peak.age[,i]=-career.sim$sims.list$beta[,i,2]/2/ career.sim$sims.list$beta[,i,3]
}  

dimnames(peak.age)[[2]]=c("Aaron","Greenberg", "Killebrew", "Mantle","Mays", "McCovey" ,"Ott", "Ruth", "Schmidt", "Sosa")

densityplot(as.mcmc(peak.age),plot.points=FALSE)

summary(as.mcmc(peak.age))
    