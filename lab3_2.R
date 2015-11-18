options(digits = 20)

## Test approximation for x = 0.5
x <- 0.5

# Float precision
# It looks like k = 20 is enough
# error is 0.00000012111394387392948601700
for (k in seq(1, 50)){
  err <- abs(log(x) - logApproxFloat(x, k))
  print(sprintf('n = %3d, error = %30.29f', k, err))
}

# Double precision
# It looks like k = 45 is enough
# error is 0.00000000000000022204460492503
for (k in seq(1, 50)){
  err <- abs(log(x) - logApproxDouble(x, k))
  print(sprintf('n = %2d, error = %30.29f', k, err))
}

# Double precision, Kahan's summation is used
# It looks like k = 45 is enough
# error is 0.00000000000000000000000000000
for (k in seq(1, 50)){
  err <- abs(log(x) - logApproxDouble2(x, k))
  print(sprintf('n = %2d, error = %30.29f', k, err))
}

## Test approximation for x = 0.01
x <- 0.01

# Float precision
# It looks like k = 846 is enough
# error is 0.00001710218682138275880788569
for (k in seq(1, 1000)){
  err <- abs(log(x) - logApproxFloat(x, k))
  print(sprintf('n = %4d, error = %30.29f', k, err))
}

# Double precision
# It looks like k = 2730 is enough
# error is 0.00000000000003197442310920451
for (k in seq(1, 3000)){
  err <- abs(log(x) - logApproxDouble(x, k))
  print(sprintf('n = %4d, error = %30.29f', k, err))
}

# Double precision, Kahan's summation is used
# It looks like k = 2997 is enough
# error is 0.00000000000000266453525910038
for (k in seq(1, 3000)){
  err <- abs(log(x) - logApproxDouble2(x, k))
  print(sprintf('n = %4d, error = %30.29f', k, err))
}

## R versions of Rcpp functions
# direct summation
logapproxR1 <- function(x, k){
  x = 1.0 - x
  s = 0	
  for(i in seq(1, k)){
    term = x^i
    s = s - 1.0 / i * term
  }
  return (s)
}

# vectorized summation
logapproxR2 <- function(x, k){
  x = 1.0 - x
  i <- seq(1, k)
  term = -x^i/i
  s = sum(term)
  return (s)
}

# compile R functions
logapproxR1cmp <- compiler::cmpfun(logapproxR1)
logapproxR2cmp <- compiler::cmpfun(logapproxR2)

# Test time of computations
x <- 0.5
k <- 45
microbenchmark::microbenchmark(
  logApproxDouble1(x, k), 
  logapproxR1cmp(x, k),
  logapproxR2cmp(x, k))

# Test time of computations
x <- 0.01
k <- 3000
microbenchmark::microbenchmark(
  logApproxDouble1(x, k), 
  logapproxR1cmp(x, k),
  logapproxR2cmp(x, k))