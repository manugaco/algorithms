# This function creates a squared matrix with the following characteristics:

# Input: parameter of dimensions (k)
# Algorithm: given the number of dimensions the first row is a vector (v) of size k+1 of ascending integers from 0 to k.
# Following rows are same length vectors as the previous vector but each new row equals to the t+1 lagged previous vector.
# Output: Squared matrix (kxk) with recursive integers on each row.

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