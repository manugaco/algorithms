# K-means implementation in R.
# - The input of the function takes the following arguments:
#     - Features dataframe.
#     - Number of centroids "k".
#     - Number of iterations to compute, (default 100).
#     - Type of distance (euclidean, squared euclidean or manhattan).
# - The output of the function is:
#     - Dataframe with original data and the associated centroid.

#Libraries:

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
  while (iter < niter) {
    df_dist <- data.frame()

    #Compute distances between centroids and datapoints:
    for (i in seq_len(nrow(data))) {
      dist <- c()
      for (j in seq_len(length(cent))) {

        #Distance metric:
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
        df_dist <- rbind(df_dist, dist)
        colnames(df_dist) <- c("1", "2", "3")
      }
    data_c <- cbind(df_dist, colnames(df_dist)[apply(df_dist, 1, which.min)])
    colnames(data_c) <- c("1", "2", "3", "Clusters")

    #Recompute centroids by mean:
    if (nrow(df_cent) == k) {
      df_cent <- data_c %>%
      group_by(Clusters) %>%
      summarise(across(everything(), mean))
      cent <- list()
      for (i in seq_len(nrow(df_cent))) {
        cent[[i]] <- unlist(df_cent[i, c("1", "2", "3")], use.names = FALSE)
      }
    }
    iter <- iter + 1
  }
  return(data_c)
}

#Iris Dataset Example:

data(iris)
data <- iris[, 1:4]
y_test <- iris$Species

results <- kmeans_manu(data, k = 3, niter = 10, metric = "euclidean")
table(results$Clusters, y_test)