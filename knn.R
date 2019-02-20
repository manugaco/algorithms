# We are going to use the iris domain (plant classification)
# iris data is contained in a data.frame, but in Rcpp it is easier to use matrices
# given that all attributes are numeric, 

# X contains the inputs as a matrix of real numbers
X <- as.matrix(iris[,1:4])

# y contains the outputs (1, 2, 3, or 4) in integer format
y <- as.integer(iris[,5])

# This is the point we want to classify
X0 <- c(5, 4, 1.2, 0.5)


###############################################################################
# Code for KNN starts here. You have to translate this code into C++ / Rcpp

my_knn_R = function(X, X0, y){

  nrows = nrow(X)
  ncols = ncol(X)
  
  min_distance = 99999999
  min_class = -1
  
  for(i in 1:nrows){
  
    distance = 0
    for(j in 1:ncols){
      difference = X[i,j]-X0[j]
      distance = distance + difference * difference
    }
    # No need to compute sqrt(distance)
    if(distance < min_distance){
      min_distance = distance
      min_class = y[i]
    }
  }
  min_class
}

# Code for KNN ends here. 
##############################################################################

# Using knn to classify point X0

library(Rcpp)

sourceCpp("knn.cpp")

library(class)

#Verify if any point is being classified correctly

table <- matrix(0, ncol=4, nrow=1000)

for(i in 1:1000){
  X0 <- runif(4, 0, 10)
  table[i, 1] <- knn(X, X0, y)
  table[i, 2] <- my_knn_R(X, X0, y)
  table[i, 3] <- my_knn_c(X, X0, y)
  table[i, 4] <- my_knn_c2(X, X0, y)
}

table[,1] == table[,2]
table[,1] == table[,3]
table[,1] == table[,4]

library(microbenchmark)

marks <- microbenchmark(
        knn(X, X0, y),
        my_knn_R(X, X0, y),
        my_knn_c(X, X0, y),
        my_knn_c2(X, X0, y))

library(dplyr)

marks_df <- data.frame(marks)

test_df <- marks_df %>%
  group_by(expr) %>%
      summarize(mean(time/1000))

knn

library(reticulate)





