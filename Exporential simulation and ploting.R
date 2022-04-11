exporential <- function(n,l) {
  u <- runif(n)
  rez = -(log(1-u))/l
  return(rez)
}

data <- exporential(1000,0.5)

hist(data, 50, freq = FALSE)


