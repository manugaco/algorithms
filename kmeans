#My kmeans algorithm

##Algorithm description

#kmeans algorithm belongs to the family of unsupervised machine learning algorithms
#The purpose of it is to find groups of similar features on data, without previous information

##Behaviour

#First of all, the algorithm sets a subspace taking the limits of each variable as bounds (x1, x2, ... , xp)
#Then, k (number of centroids) points are selected randomly inside this subspace.
#The next step is to compute the distances between each centroid and all the points of the dataset.
#Then, I relate each point to the closest centroid, creating a cluster, using a distance measurement.
#Once all the points are related to its respective centroid, I compute the mean point for each variable in each cluster.
#And I get a new position for the centroid.
#This process is repeated n times (can be improved)
#It can happen that a centroid may not find any point close to it, to solve this, I repeate the process of launching centroids
m times, and I choose that configuration with the minimum distance at the end of the previous process
#This procedure also gives me a better best accurary computing the centroids

##Features

#The algorithm can compute clusters for p dimensions matrices, but it will only plot the clusters if the input is bidimensional
#The number of centroids by default is 2, but the parameter can be changed to any number
#It has three kinds of distances measurements (Euclidean, Euclidean squared and Manhattan)
#The number of iterations is set to 50 by default
#It has also initial configurations in order to launch the optimal position of centroids at the beginning

#Future improvements

#Stop the while loop when the centroids do not move.
#Optimal number of centroids (elbow chart)
#Dynamic chart using plotly

kmeans_manu <- function(input, k=2, method = "Euclidean", iter = 50, config = 10){
  
  require(ggplot2)
  require(dplyr)
  
  #k is set to 2 by default
  if(k<2){
    stop("Number of clusters must be higher than 1")
  }
  
  if(ncol(input) > 2){
    print("The results will not be displayed")
  }
  options(warn=-1)
  #Computing centroids
  
  #Defining subspace
  
  subspace_tab <- matrix(0, ncol=ncol(input), nrow = 2)
  for(i in 1:ncol(input)){
  subspace_tab[1,i] <- min(input[,i])
  subspace_tab[2,i] <- max(input[,i])
  }
  
  mean_col <- function(m){
  for(i in 1:ncol(m)){
    new[i] <- mean(m[,i])
  }
  new <<- new
}
  
  for(n in 1:config){
    
  #Initializing objects
    
  centroids_ini <- matrix(0, nrow=k, ncol=ncol(input))
  list_centroids <- list()
  distances <- data.frame(matrix(0, nrow = nrow(input), ncol = k))
  names <- vector(mode = "numeric", length = length(k))
  colcent <- data.frame(matrix(0, ncol = 1, nrow = nrow(input)))
  colnames(colcent) <- "Centroid"
  inputdf <- data.frame(input)
  new <- matrix(0, nrow= 1, ncol= ncol(input))

  
  #Computing centroids
    for(i in 1:ncol(subspace_tab)){
      for(j in 1:nrow(centroids_ini)){
      centroids_ini[,i] <- runif(k, min=min(subspace_tab[,i]), max=max(subspace_tab[,i]))
      }
    }
  
  #Main loop
  
  n_iter=0
  while(n_iter <= iter){
  
  #Distances
  
  if(method == "Euclidean"){
    for(i in 1:nrow(inputdf)){
      for(j in 1:nrow(centroids_ini)){
        distances[i,j] <- sqrt(sum((input[i,] - centroids_ini[j,])^2))
      }
    }
  } else if(method == "Euclidean_sq"){
    for(i in 1:nrow(inputdf)){
      for(j in 1:nrow(centroids_ini)){
        distances[i,j] <- sum((input[i,] - centroids_ini[j,])^2)
      }
    }
  } else if(method == "Manhattan"){
    for(i in 1:nrow(inputdf)){
      for(j in 1:nrow(centroids_ini)){
        distances[i,j] <- sum(abs(input[i,] - centroids_ini[j,]))
      }
    }
  }
  
  #Naming the distances matrix
  
  for(i in 1:k){
   names[i] <- paste("k", i, sep="")
  }
  colnames(distances) <- names
  
  #Assigning the points to centroids
  
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
  
  inputdf <- data.frame(cbind(input,colcent), stringsAsFactors = TRUE)

  #New centroids
  
  #Computing means for each element of the list, and storing it in a new centroid matrix
  centroids_ini <- matrix(0, nrow=nrow(centroids_ini), ncol=ncol(centroids_ini))
  list_centroids <- split(inputdf, f=inputdf$Centroid)
  
  if(length(list_centroids) < k){
    break
  }else if(length(list_centroids) == k){
    for(i in 1:k){
    dot <- matrix(as.numeric(as.character(unlist(list_centroids[[i]]))), ncol = ncol(inputdf))
    dot <- dot[,-c(length(inputdf))]
    centroids_ini[i,] <- mean_col(dot)
    }
  }
  #Finished iterations
  n_iter = n_iter + 1
  }
  
  #Computing minimum distance of each configuration
  
    dist_aux <- sum(distances)
    coord_aux <- centroids_ini
  
  if(n == 1L){
    dist <- dist_aux
    centroids_fin <- coord_aux
  }
  
  if(dist > dist_aux){
    dist <- dist_aux
    centroids_fin <- coord_aux
  }
  
}#Optimization of the centroids initial position
  
#Launching Kmeans with optimal centroids  
  
  #Initializing all objects
  
  n_iter = 0
  distances <- data.frame(matrix(0, nrow = nrow(input), ncol = k))
  inputdf2 <- data.frame(input)
  
  #First plot
  if(ncol(input) == 2){
    p1 <- ggplot(inputdf2, aes(x=X1, y=X2)) +
      geom_point() +
      ggtitle("Input data")
    
    print(p1)
  }
  
  while(n_iter <= iter){
    
    #Distances
    
    if(method == "Euclidean"){
      for(i in 1:nrow(inputdf2)){
        for(j in 1:nrow(centroids_fin)){
          distances[i,j] <- sqrt(sum((input[i,] - centroids_fin[j,])^2))
        }
      }
    } else if(method == "Euclidean_sq"){
      for(i in 1:nrow(inputdf2)){
        for(j in 1:nrow(centroids_fin)){
          distances[i,j] <- sum((input[i,] - centroids_fin[j,])^2)
        }
      }
    } else if(method == "Manhattan"){
      for(i in 1:nrow(inputdf2)){
        for(j in 1:nrow(centroids_fin)){
          distances[i,j] <- sum(abs(input[i,] - centroids_fin[j,]))
        }
      }
    }
    
    #Naming the distances matrix
    names <- vector(mode = "numeric", length = length(k))
    for(i in 1:k){
      names[i] <- paste("k", i, sep="")
    }
    colnames(distances) <- names
    
    #Assigning the points to centroids
    
    colcent <- data.frame(matrix(0, ncol = 1, nrow = nrow(input)))
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

generator1 <- function(){
  n1 <- sample(50:200, 1)
  mu1<-c(runif(1, -100, 100), runif(1, -100, 100))
  Sigma1<-matrix(c(sample(15:50, 1),10,10,sample(15:50, 1)),2,2)
  n2 <- sample(50:200, 1)
  mu2<-c(runif(1, -100, 100), runif(1, -100, 100))
  Sigma2<-matrix(c(sample(15:50, 1),-10,-10,sample(15:50, 1)),2,2)
  n3 <- sample(50:200, 1)
  mu3<-c(runif(1, -100, 100), runif(1, -100, 100))
  Sigma3<-matrix(c(sample(15:50, 1),15,15,sample(15:50, 1)),2,2)
  n4 <- sample(50:200, 1)
  mu4<-c(runif(1, -100, 100), runif(1, -100, 100))
  Sigma4<-matrix(c(sample(15:50, 1),5,5,sample(15:50, 1)),2,2)
  
  library(MASS)
  
  y1<-mvrnorm(n1, mu1, Sigma1)
  y2<-mvrnorm(n2, mu2, Sigma2)
  y3<-mvrnorm(n3, mu3, Sigma3)
  y4<-mvrnorm(n4, mu4, Sigma4)
  input <<-rbind(y1, y2, y3, y4)
   plot <- plot(input)
   print(plot)
}

generator1()

kmeans_manu(input)
