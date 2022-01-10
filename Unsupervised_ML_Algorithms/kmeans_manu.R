# K-means implementation in R.
# - The input of the function takes the following arguments:
#     - Features dataframe.
#     - Number of centroids "k".
#     - Number of iterations to compute, (default 100).
#     - Type of distance (euclidean, squared euclidean or manhattan).
# - The output of the function is:
#     - Dataframe with original data and the associated centroid.

#Libraries:

require(dplyr)
options(warn = -1)

#k-means Function:

kmeans_manu <- function(data, k, niter = 50, metric = "Euclidean") {

  #k value sanity check:
  if (k < 2) {
    stop("Number of clusters must be higher than 1")
    }

  #Algorithm body:
  #Initial centroids:

  cent <- list()
  #Initial centroids
    for (i in seq_len(k)) {
      cent_coord <- c()
      for (j in seq_len(ncol(data))) {
        cent_coord[j] <- runif(1,
                              min = min(data[, j]),
                              max = max(data[, j]))
      }
      cent[[i]] <- cent_coord
    }

  #Distances from observations to centroids:
  iter <- 0
  while (iter < n_iter) {
      dist_ls <- list()

    #Compute distances between centroids and datapoints:
    for (i in seq_len(nrow(inputdf))) {
      dist <- c()
      for (j in seq_len(nrow(centroids_ini))) {
        if (metric == "euclidean") {
          dist[j] <- sqrt(sum((data[i, ] - cent[[j]])^2))
          }
        if (metric == "euclidean squared") {
          dist[j] <- sum((data[i, ] - cent[[j]])^2)
          }
        if (metric == "manhattan") {
          dist[j] <- sum(abs(data[i, ] - cent[[j]]))
          }
        }
        dist_ls[[i]] <- dist
      }
    n_iter <- n_iter + 1
  }
  return(data_c)
}

#Iris Dataset Example:

data(iris)
data <- iris[, 1:4]
y_test <- iris$Species

results <- kmeans_manu(input)
