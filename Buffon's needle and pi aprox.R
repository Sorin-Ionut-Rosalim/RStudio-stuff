buffon <- function(n) {
  x <- runif(n, 0, 1/2)
  u <- runif(n, 0, pi/2)
  counter <- 0
  
  for (i in 1:n){
    if ( x[i] <= (1/2) * sin(u[i]) )
      counter <- counter + 1
  }
  return (counter/n)
}



buffon_pi <- function(n) {
  x <- runif(n, 0, 1/2)
  u <- runif(n, 0, pi/2)
  counter <- 0
  
  for (i in 1:n){
    if ( x[i] <= (1/2) * sin(u[i]) )
      counter <- counter + 1
  }
  return ((2*n/counter))
}
