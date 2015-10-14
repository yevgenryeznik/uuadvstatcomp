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
