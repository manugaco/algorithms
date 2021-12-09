# This function works in the following way:
# First, it creates a matrix with all the possible combinations of the grouping variable levels.
# Then, it filters all the rows that match every combination and it stores in an independent matrix.
# it uses the package prodlim to match the row combinations.


splitDF <- function(data, group){
  
  data <- as.matrix(data)
  
  if (!require("prodlim")) install.packages("prodlim")
  library(prodlim)
  
  #Compute length of final list (all possible combinations)
  
  combs <- unique(data[,group])
  combs <- as.matrix(na.omit(combs))
  
  data_ls <- list()
  
  for(i in 1:nrow(combs)){
    data_ls[[i]] <- data[which(row.match(combs[i,],data[,group]) == 1),]
    data <- data[-which(row.match(combs[i,],data[,group]) == 1),] #This reduces the search time
    names(data_ls)[i] <- paste(combs[i,1], combs[i,2], sep = " & ")
  }
  return(data_ls)
}

data("iris")
data <- iris

class <- rbinom(nrow(iris), 5, 0.5)
sex <- rbinom(nrow(iris), 1, 0.5)
data <- cbind(data, class, sex)
group <- c("Species", "class", "sex")

#Benchmark

fin <- list()

for(i in 1:50){
  ini <- Sys.time()
  df <- splitDF(data, group)
  fin[i] <- Sys.time() - ini
}
