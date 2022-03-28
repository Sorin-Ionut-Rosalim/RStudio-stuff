bernoulli <- function(p){
  u = runif(1,0,1) #uniform random variable
  if(u < 1 - p){
    return(0)
  }
  else return(1)
    
}

bernoulli2 <- function(p){
  u = runif(1,0,1) #uniform random variable
  return(as.integer(u >= 1-p))
}

binomial <- function(n,p){
  library(gtools)
  u = runif(1,0,1)
  s = (1-p)^n
  i = 0
  while(s<=u){
    i = i + 1
    s = s + ((1-p) ^ (n-1)) * p^i * dim(combinations(n,i))[1]
  }
  return(i)
}

geometric <- function(p){
  u = runif(1,0,1)
  i = 1
  s = (1-p)^(i-1) * p
  while(s<=u){
    i = i + 1
    s = s + (1-p)^(i-1) * p
  }
  return(i)
}

