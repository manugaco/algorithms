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

  subspace_tab <- matrix(0, ncol = ncol(data), nrow = 2)
  for (i in seq_len(ncol(data))) {
  subspace_tab[1, i] <- min(data[, i])
  subspace_tab[2, i] <- max(data[, i])
  }

  mean_col <- function(m) {
  for (i in seq_len(ncol(m))) {
    new[i] <- mean(m[, i])
  }
  new <<- new
}

  for (n in 1:config) {

  #Initializing objects

    centroids_ini <- matrix(0, nrow = k, ncol = ncol(data))
    list_centroids <- list()
    distances <- data.frame(matrix(0, nrow = nrow(data), ncol = k))
    names <- vector(mode = "numeric", length = length(k))
    colcent <- data.frame(matrix(0, ncol = 1, nrow = nrow(data)))
    colnames(colcent) <- "Centroid"
    inputdf <- data.frame(data)
    new <- matrix(0, nrow = 1, ncol = ncol(data))

  #Initial centroids
    for (i in seq_len(ncol(subspace_tab))) {
      for (j in seq_len(nrow(centroids_ini))) {
      centroids_ini[, i] <- runif(k,
                                  min = min(subspace_tab[, i]),
                                  max = max(subspace_tab[, i]))
      }
    }

  #Main loop
  iter <- 0
  while (iter < n_iter) {

  #Distances
  if (method == "Euclidean") {
    for (i in seq_len(nrow(inputdf))) {
      for (j in seq_len(nrow(centroids_ini))) {
        distances[i, j] <- sqrt(sum((data[i, ] - centroids_ini[j, ])^2))
      }
    }
  } else if (method == "Euclidean_sq") {
    for (i in seq_len(nrow(inputdf))) {
      for (j in seq_len(nrow(centroids_ini))) {
        distances[i, j] <- sum((data[i, ] - centroids_ini[j, ])^2)
      }
    }
  } else if (method == "Manhattan") {
    for (i in seq_len(nrow(inputdf))) {
      for (j in seq_len(nrow(centroids_ini))) {
        distances[i, j] <- sum(abs(data[i, ] - centroids_ini[j, ]))
      }
    }
  }

  #Naming the distances matrix
  for (i in 1:k) {
   names[i] <- paste("k", i, sep = "")
  }
  colnames(distances) <- names

  #Assigning the points to centroids
  for (i in seq_len(nrow(distances))) {
      colcent[i, ] <- min(distances[i, ])
  }

  for (i in seq_len(ncol(distances))) {
    for (j in seq_len(nrow(distances))) {
      if (colcent[j, 1] == distances[j, i]) {
        colcent[j, 1] <- paste("k", i, sep = "")
      }
    }
  }

  inputdf <- data.frame(cbind(data, colcent), stringsAsFactors = TRUE)

  #New centroids

  #Computing means for each element of the list,
  #and storing it in a new centroid matrix
  centroids_ini <- matrix(0,
                          nrow = nrow(centroids_ini),
                          ncol = ncol(centroids_ini))
  list_centroids <- split(inputdf, f = inputdf$Centroid)

  if (length(list_centroids) < k) {
    break
  }else if (length(list_centroids) == k) {
    for (i in 1:k) {
    dot <- matrix(
      as.numeric(
        as.character(
          unlist(list_centroids[[i]]
          )
          )
          ),
          ncol = ncol(inputdf))
    dot <- dot[, -c(length(inputdf))]
    centroids_ini[i, ] <- mean_col(dot)
    }
  }
  #Finished iterations
  n_iter <- n_iter + 1
  }

  #Computing minimum distance of each configuration

    dist_aux <- sum(distances)
    coord_aux <- centroids_ini

  if (n == 1L) {
    dist <- dist_aux
    centroids_fin <- coord_aux
  }

  if (dist > dist_aux) {
    dist <- dist_aux
    centroids_fin <- coord_aux
  }

}#Optimization of the centroids initial position

#Launching Kmeans with optimal centroids

  #Initializing all objects

  n_iter <- 0
  distances <- data.frame(matrix(0, nrow = nrow(data), ncol = k))
  inputdf2 <- data.frame(data)

  #First plot
  if (ncol(input) == 2) {
    p1 <- ggplot(inputdf2, aes(x = X1, y = X2)) +
      geom_point() +
      ggtitle("Input data")
    print(p1)
  }

  while (n_iter <= iter) {

    #Distances
    if (method == "Euclidean") {
      for (i in seq_len(nrow(inputdf2))) {
        for (j in seq_len(nrow(centroids_fin))) {
          distances[i, j] <- sqrt(sum((data[i, ] - centroids_fin[j, ])^2))
        }
      }
    } else if (method == "Euclidean_sq") {
      for (i in seq_len(nrow(inputdf2))) {
        for (j in seq_len(nrow(centroids_fin))) {
          distances[i, j] <- sum((data[i, ] - centroids_fin[j, ])^2)
        }
      }
    } else if (method == "Manhattan") {
      for (i in seq_len(nrow(inputdf2))) {
        for (j in seq_len(nrow(centroids_fin))) {
          distances[i, j] <- sum(abs(data[i, ] - centroids_fin[j, ]))
        }
      }
    }

    #Naming the distances matrix
    names <- vector(mode = "numeric", length = length(k))
    for (i in 1:k) {
      names[i] <- paste("k", i, sep = "")
    }
    colnames(distances) <- names

    #Assigning the points to centroids

    colcent <- data.frame(matrix(0, ncol = 1, nrow = nrow(data)))
    colnames(colcent) <- "Centroid"

    for(i in 1:nrow(distances)){
      colcent[i,] <- min(distances[i,])
    }
    
    for(i in 1:ncol(distances)){
      for(j in 1:nrow(distances)){
        if(colcent[j,1] == distances[j,i]){
          colcent[j,1] <- paste("k", i, sep = "")
        }
      }
    }
    
    inputdf2 <- data.frame(cbind(input,colcent), stringsAsFactors = TRUE)
    
    levels <- matrix(0, ncol=k)
    for(i in 1:k){
      levels[i] <- paste("k", i, sep="")
    }
    
    inputdf2$Centroid <- factor(inputdf2$Centroid, levels = levels, labels = levels)
    #plot primary centroids
    if(ncol(input) == 2){
      centroids_fin <- data.frame(centroids_fin, stringsAsFactors = TRUE)
      ppc <- ggplot() +
        geom_point(data = inputdf2, aes(x=X1, y=X2, color = Centroid)) +
        geom_point(data = centroids_fin, aes(x=X1, y=X2)) +
        ggtitle("Kmeans by manugaco")
      
      print(ppc)
    }
    #New centroids
    
    #Means for columns function
    new <- matrix(0, nrow= 1, ncol= ncol(input))
    mean_col <- function(m){
      for(i in 1:ncol(m)){
        new[i] <- mean(m[,i])
      }
      new <<- new
    }
    
    #Computing means for each element of the list, and storing it in a new centroid matrix
    centroids_fin <- matrix(0, nrow=nrow(centroids_fin), ncol=ncol(centroids_fin))
    list_centroidsf <- split(inputdf2, f=inputdf2$Centroid)
    
    for(i in 1:length(list_centroidsf)){
      dot <- matrix(as.numeric(as.character(unlist(list_centroidsf[[i]]))), ncol = ncol(inputdf2))
      dot <- dot[,-c(length(inputdf2))]
      centroids_fin[i,] <- mean_col(dot)
    }
    
    #Final iteration plot
    
    if(ncol(input) == 2){
      centroids_fin <- data.frame(centroids_fin, stringsAsFactors = TRUE)
      colnames(centroids_fin) <- c("X1", "X2")
      pfc <- ggplot() +
        geom_point(data = inputdf2, aes(x=X1, y=X2, color = Centroid)) +
        geom_point(data = centroids_fin, aes(x=X1, y=X2)) +
        ggtitle("Kmeans by manu_gaco")
      
      print(pfc)
    }
    
    #Finished iteration
    n_iter = n_iter + 1
  }
  
} #function

#In order to test the algorithm, I am going to use a random clusters generator, using a multinomal distribution.



kmeans_manu(input)
