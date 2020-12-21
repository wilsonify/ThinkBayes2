make_post <- function(y, shape, scale) {
  function(x) {
    dgamma(x, shape = y + shape,
           scale = 1 / (1 + 1 / scale))
  }
}

set.seed(2017 - 11 - 29)
y <- 2
prior.shape <- 3
prior.scale <- 3
p <- make_post(y, prior.shape, prior.scale)
curve(p, 0, 12, n = 1000, lwd = 3, xlab = expression(mu),
      ylab = expression(paste("p(", mu, " | y)")))
curve(dgamma(x, shape = prior.shape, scale = prior.scale), add = TRUE,
      lty = 2)
legend("topright", legend = c("Posterior", "Prior"), lty = c(1, 2), lwd = c(3, 1), bty = "n")

pmode <- (y + prior.shape - 1) * (1 / (1 + 1 / prior.scale))
pmode

pmean <- (y + prior.shape) * (1 / (1 + 1 / prior.scale))
pmean

a <- prior.shape
b <- prior.scale
fhat <- deriv3(~mu^(y + a - 1) * exp(-mu * (1 + 1 / b)) / ((1 / (1 + 1 / b))^(y + a) * gamma(y + a)), "mu", function.arg = TRUE)

post.shape <- y + prior.shape - 1
post.scale <- 1 / (length(y) + 1 / prior.scale)
lapprox <- Vectorize(function(mu, mu0 = pmode) {
  deriv <- fhat(mu0)
  grad <- attr(deriv, "gradient")
  hess <- drop(attr(deriv, "hessian"))
  f <- function(x) dgamma(x, shape = post.shape, scale = post.scale)
  hpp <- (hess * f(mu0) - grad^2) / f(mu0)^2
  exp(log(f(mu0)) + 0.5 * hpp * (mu - mu0)^2)
}, "mu")

curve(p, 0, 12, n = 1000, lwd = 3, xlab = expression(mu),
      ylab = expression(paste("p(", mu, " | y)")))
curve(dgamma(x, shape = prior.shape, scale = prior.scale), add = TRUE,
      lty = 2)
legend("topright",
       legend = c("Posterior Density", "Prior Density", "Laplace Approx"),
       lty = c(1, 2, 1), lwd = c(3, 1, 1), col = c(1, 1, 2), bty = "n")
curve(lapprox, 0.001, 12, n = 1000, add = TRUE, col = 2, lwd = 2)