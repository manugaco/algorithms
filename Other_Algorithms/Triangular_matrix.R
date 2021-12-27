# This function creates a matrix with the 

trimat <- function(k){

  num <- rep(0:k, times=k)
  mat <- matrix(num, nrow=k+1, ncol=k+1, byrow=TRUE)
  for(i in 2:ncol(mat)){
    mat[i,] <- mat[(i-1),]+1
    }

  for(i in 1:ncol(mat)){
    for(j in 1:ncol(mat)){
      if(mat[i,j]> k){
        mat[i,j] <- mat[i,j]-(k+1)
      }
    }
  }
print(mat)
}

trimat(6)