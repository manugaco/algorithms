# Description: Simple function to compute the power of a matrix in R programming.

matpow <- function(m, p){

  ## Arguments:

  # m = squared matrix.
  # p = desired power to rise the matrix.

  #Sanity check: Matrix must be squared.

  if(nrow(m)!=ncol(m)){
    print("Error: matrix is not squared")
  }

  #Function body:

  vec_aux <- list()
  vec_aux[[1]] <- m
  for(i in 2:p){
    vec_aux[[i]] <- m %*% vec_aux[[i-1]]
  }

  #The function displays the output if the object is not assigned:
  return(vec_aux[[p]])

  #The function can store the result in an object:
  output <<- vec_aux[[p]]
}

# Testing the function:

mat <- matrix(c(1, 2, 1, 1, 3, 4, 1, 2, 3), ncol=3, nrow=3, byrow=T)

matpow(mat, 3)