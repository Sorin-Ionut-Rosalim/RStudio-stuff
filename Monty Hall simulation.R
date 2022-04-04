monty <- function(n){
  k = 0
  for(i in 1:n){
    car_door <- sample(1:3, 1)#the door which has a car behind
    guess <- sample(1:3, 1)#initial guess  
    if( car_door == 1)
      monty <- sample(2:3, 1)
    else 
      if ( car_door == 3)
        monty <- sample(1:2, 1)
    else {
      monty <- sample(0:1, 1)
      if ( monty == 0)
        monty = 1
      else
        monty = 3
    }
    if(car_door != guess)
        k = k + 1
  }
  
  return(k/n)
  #return(ifelse(car_door!=guess, "Switch", "Stick"))
}
monty(1000)
