## optimization part -------------------------------------------------
ofv <- function(x){ # function to be optimized
  (x - 3)^2 + 2*(x - 3)^2 + 3 * (x - 15)^2 + sin(100*x)
}

do.opt <- function(interval, fun = ofv){ # optimization 
  sol <- optimise(fun, interval)
  out <- sprintf("x.min = %f, ofv = %f", sol$minimum, sol$objective)
  return(out)
}

lapply(list(c(0, 100), c(0, 15), c(9, 12), c(10, 11)), do.opt)

## integration part --------------------------------------------------
int.compareness <- function(fun, left, right, subdiv = 1e7, subint = 15){ # compare different approaches
  do.int <- function(interval, fcn = fun){ # integration
    return(integrate(fcn, lower = interval[1], upper = interval[2], subdivisions = subdiv)$value)
  }
  print("----- direct integration -----")
  print(sprintf("integral value = %f", do.int(c(left, right))))
  print("system time")
  print(system.time(do.int(c(left, right))))

  print("----- integration with lapply -----")
  interval <- seq(left, right, length = subint)
  interval <- cbind(interval[-length(interval)], interval[-1])
  interval <- split(interval, row(interval))
  
  print(sprintf("integral value = %f", sum(as.numeric(lapply(interval, do.int)))))
  print("system time")
  print(system.time(sum(as.numeric(lapply(interval, do.int)))))

  print("----- integration with parallelization -----")
  library(parallel)
  cl.number <- c(1, 2, 4, 8)
  for (n in cl.number){
    cl <- makePSOCKcluster(n)
    print(sprintf("number of clusters = %i", n))
    print(sprintf("integral value = %f", sum(as.numeric(parLapply(cl, interval, do.int, fcn = fun)))))
    print("systme time")
    print(system.time(sum(as.numeric(parLapply(cl, interval, do.int, fcn = fun)))))
  }
}

# test for given function: x*sin(x)
int.compareness(function(x){x*sin(x)}, left = -7e5, right = 7e5)

# test for my example: exp(x^2)
int.compareness(function(x){sin(x^2)}, left = 0, right = 500, subint = 50)

## memoisation part ---------------------------------------------------------
fib <- function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}

fib2 <- memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})

fib3 <- memoise(fib)

## ggplot2 part -----------------------------------------------------
qplot(carat, price, data = diamonds)
qplot(carat, price, data = diamonds, color = color)
diam.plt <- qplot(carat, price, data = diamonds, color = color, geom = c("point", "smooth")) 
diam.plt + scale_colour_brewer(name = "New Legend")
qplot(carat, price, data = diamonds, color = color, geom = "boxplot")

