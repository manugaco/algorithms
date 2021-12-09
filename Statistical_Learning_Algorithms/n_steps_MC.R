n_steps_MC <- function(alpha, tm, labels = 0, t = 50) {
  
  #Arguments:
  
  #alpha is th vector of initial probabilities
  #tm is the transition matrix
  #label is a vector with the labels of each state
  #t number of steps
  
  #Checking the sizes of the arguments
  
  if(length(labels)!=ncol(tm) && ncol(tm) != nrow(tm)){
    print("Error: Size of the arguments is not consistent")
    break()
  }
  
  #Checking the sum of the probabilities vector is 1
  
  if(sum(alpha)!=1){
    print("Error: The initial probabilities vector does not add 1")
    break()
  }
  
  #Checking if the transition matrix is stochastic
  check <- 0
  for(i in 1:nrow(tm)){
    check[i] <- sum(tm[i,])
  }
  if(check[i]!=1){
    print("Error: The transition matrix is not stochastic")
    break()
  }
  
  #Checking the states vector is set by default, otherwise it is created by default
  
  if(length(labels) == 0){
    print("No states vector, it will be set by default")
    label <- 0
    for(i in 1:nrow(tm)){
      label[i] <- paste("State", i, sep="")
    }
  }else{
    label <- labels
  }
  
  #Inizializating objects
  
  #Number of steps
  steps <- numeric(t)
  
  #Simulating the first step X_o
  steps[1] <- which(rmultinom(1, 1, alpha) == 1)
  
  #Simulating the t steps
  for(t in 2:t) {
    p  <- tm[steps[t-1], ]
    steps[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  out <- 0
  
  #Giving labels to each state
  for(i in 1:length(steps)){
      out[i] <- label[steps[i]]
  }
  
  return(out)
}

#Example

Mat <- matrix(c(0.5, 0.3, 0.2,
                0.25, 0.25, 0.25,
                0.35, 0.45, 0.2), 
                ncol = 3, byrow=TRUE)

alpha <- c(1,0,0)
labels <- c("Rainy", "Sunny", "Cloudy")

n_steps_MC(alpha, Mat, labels)
