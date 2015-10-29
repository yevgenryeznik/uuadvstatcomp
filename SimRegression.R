x <- rnorm(20, mean = 2, sd = 1.5)

simNumber <- 10000
beta0 <- c()
beta1 <- c()
for(r in 1:simNumber){
  y <- 5 + 2*x + 1.5*rexp(length(x))
  cf <- lm(y ~ x)$coefficients
  beta0 <- c(beta0, cf[1])
  beta1 <- c(beta1, cf[2])
}

par(mfrow = c(1, 2))
hist(beta0)
hist(beta1)