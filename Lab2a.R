ofv <- function(x){
  (x - 3)^2 + 2*(x - 3)^2 + 3 * (x - 15)^2 + sin(100*x)
}

do.opt <- function(interval, fun = ofv){
  sol <- optimise(ofv, interval)
  out <- sprintf("x.min = %f, ofv = %f", sol$minimum, sol$objective)
  return(out)
}


