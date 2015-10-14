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
int.compareness <- function(fun, left, right, n = 15){ # compare different approaches
  do.int <- function(interval, fcn = fun){ # integration
    return(integrate(fcn, lower = interval[1], upper = interval[2], subdivisions = 1e7)$value)
  }
  print("----- direct integration -----")
  print(sprintf("integral value = %f", do.int(c(left, right))))
  print("system time")
  print(system.time(do.int(c(left, right))))

  print("----- integration with lapply -----")
  interval <- seq(left, right, length = n)
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


