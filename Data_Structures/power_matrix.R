Simple function to compute the power of a matrix with R

matpow <- function(m, p){
  if(nrow(m)!=ncol(m)){
    print("Error: matrix is not squared")
  }
  vec_aux <- list(p)
  vec_aux[[1]] <- m
  for(i in 2:p){
    vec_aux[[i]] <- m %*% vec_aux[[i-1]]
  }
  return(vec_aux[[p]])
  output <<- vec_aux[[p]]
}
