
#The algorithm is designed to perform split validation automatically.
#it works by selecting those "k" closests points to each on the test set, and it does select and assign the most repeated category
#to the corresponding point on the test set.
#It prints the confusion matrix and some accuracy metrics if required (TRUE by default).
#If the confusion matrix is not squared, it does not print the previous information.


#I use the Iris dataset as an example.

data(iris)
data <- iris

#Implementation of knn (R)

knn_manu <- function(data, k="Auto", splitTT = 0.75, Acc = TRUE){
  
  #Avoid more than one factor variable
  
  nonum <- 0
  for(i in 1:ncol(data)){
    if(class(data[,i]) != "numeric"){
      nonum <- nonum + 1
    }
  }
  
  if(nonum > 1){
    stop("There is more than one grouping variable, select only one as input.")
  }
  
  #Detect labels variable (convert to factor)
  
  for(i in 1:ncol(data)){
    if(class(data[,i]) != "numeric"){
      data[,i] <- as.factor(data[,i])
      ind_fact <- i
    }
  }
  
  #Divide in train and test
  
  ind <- sample(1:nrow(data), size = round(splitTT*nrow(data)))
  train <- dataset[ind,]
  test <- dataset[-ind,]
  
  if (k == "Auto"){
    k <- round(sqrt(nrow(train) + nrow(test)))
  }
  
  #algorithm body
  
  distmat <- matrix(0, ncol = nrow(test), nrow = nrow(train))
  newlabs <- c()
  
  for(i in 1:nrow(test)){
    dist <- numeric()
    for(j in 1:nrow(train)){
      dist[j] <- sqrt((sum(test[i,-ind_fact] - train[j,-ind_fact]))^2)
    }
    distmat[,i] <- dist
    ord <- order(distmat[,i])
    ordind <- ord[1:k]
    data[ordind,ind_fact]
    newlabs[i] <- names(sort(table(data[ordind,ind_fact]),decreasing=TRUE)[1]) #Selected category
  }
  
  results <- list(distmat, newlabs)
  
  if(Acc == TRUE){
    if (!require("caret")) install.packages("caret")
    library(caret)
    confmat <- table(newlabs, test[,ind_fact])
    cm <- invisible(force(confusionMatrix(confmat)))
    results[[3]] <- cm
  }
  
  return(results)
} #Knn function

results <- knn_manu(iris, k = 3)
