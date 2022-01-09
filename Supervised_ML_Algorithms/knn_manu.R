
# Knn function:

knn_manu <- function(x_train, y_train, x_test, k="Auto") {

  # Args:
  # X_train = Training features set.
  # y_train = Training target labels.
  # X_test = Test Features.
  # k = Number of Neightbours.

  #k automatic selection:
  if (k == "Auto") {
      k <- round(sqrt(nrow(data)))
    }

  # Odd number of neightbours:
  if ((k %% 2) == 0) {
      k <- k + 1
  }

  #Algorithm body:
  distmat <- matrix(0, ncol = (nrow(x_train)), nrow = (nrow(x_test)))
  for (i in seq_len(nrow(x_test))) {
    dist <- numeric()
    for (j in seq_len(nrow(x_train))) {
      dist[j] <- sqrt((sum(x_test[i, ] - x_train[j, ]))^2)
      }
    distmat[i, ] <- dist
    }

  #Distances dataframe:
  df_dist <- data.frame(distmat)
  colnames(df_dist) <- rownames(x_train)
  rownames(df_dist) <- rownames(x_test)

  #Computing the predicted labels (y_hat):
  newlabs <- c()
  for (i in seq_len(nrow(x_test))) {
      keep_ind <- colnames(sort(df_dist[i, ])[, 1:k])
      vals <- data.frame(table(y_train[keep_ind, ]))
      newlabs[i] <- as.character(vals[order(vals$Freq,
                          decreasing = TRUE), ][1, 1])
  }

  return(newlabs)
}

#Iris dataset example.

data(iris)
data <- iris

ind <- sample(seq_len(nrow(iris)), size = nrow(iris) * 0.2)
train <- iris[-ind, ]
test <- iris[ind, ]
x_train <- subset(train, select = -c(Species))
y_train <- subset(train, select = c(Species))
x_test <- subset(test, select = -c(Species))
y_test <- subset(test, select = c(Species))

y_hat <- knn_manu(x_train, y_train, x_test, k = 6)
c_mat <- table(y_test$Species, y_hat, dnn = c("Actual", "Predicted"))
c_mat