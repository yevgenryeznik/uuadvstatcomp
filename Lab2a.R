# optimization part -------------------------------------------------
ofv <- function(x){ # function to be optimized
  (x - 3)^2 + 2*(x - 3)^2 + 3 * (x - 15)^2 + sin(100*x)
}

do.opt <- function(interval, fun = ofv){ # optimization 
  sol <- optimise(fun, interval)
  out <- sprintf("x.min = %f, ofv = %f", sol$minimum, sol$objective)
  return(out)
}

lapply(list(c(0, 100), c(0, 15), c(9, 12), c(10, 11)), do.opt)

# integration part --------------------------------------------------
fun <- function(x){ # function to be integrated
  return(x*sin(x))
}

do.int <- function(interval, fcn = fun){ # integration
  return(integrate(fcn, lower = interval[1], upper = interval[2], subdivisions = 1e7)$value)
}

## direct integration
# integral evaluation
print(do.int(c(-7e5, 7e5)))
# computational time evaluation
system.time(do.int(c(-7e5, 7e5)))

## integration with lapply
interval <- seq(-7e5, 7e5, by = 1e5)
interval <- cbind(interval[-length(interval)], interval[-1])
interval <- split(interval, row(interval))
# integral evaluation
print(sum(as.numeric(lapply(interval, do.int))))
# computational time evaluation
system.time(sum(as.numeric(lapply(interval, do.int))))

## integration with parallelization
library(parallel)
cl.number <- c(1, 2, 4, 8)
for (n in cl.number){
  cl <- makePSOCKcluster(n)
  # integral evaluation
  print(sum(as.numeric(parLapply(cl, interval, do.int, fcn = fun))))
  # computational time evaluation
  print(system.time(sum(as.numeric(parLapply(cl, interval, do.int, fcn = fun)))))
}
