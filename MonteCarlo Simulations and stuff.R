montecarlo_a <- function(n) {
  x <- runif(n)
  y <- runif(n)
  
  under  <- which(y <= x^2)
  no = length(under)
  
  rez  <- no / n
  return(rez)
}

montecarlo_b <- function(n) {
  x <- runif(n)
  y <- runif(n)
  
  shots <- sqrt((x-0.5)^2 + (y-0.5)^2)
  
  inside  <- which(shots<=0.5)
  no = length(inside)
  
  rez  <- 4 * no / n
  return(rez)
}

montecarlo_c <- function(n) {
  x <- runif(n)
  y <- runif(n)
  
  under  <- which( y*(x+1) <= 1)
  no = length(under)
  
  rez1  <- no / n
  
  return(rez1)
}

edrv <- function(l) {
  u <- runif(1)
  rez = -(log(1-u))
  return(rez)
}

exporential <- function(n,l) {
  x = (-1/l) * sum(runif(n))
  return(x)
}
