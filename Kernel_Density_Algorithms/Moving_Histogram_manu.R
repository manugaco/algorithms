#Kernel density estimation

#Moving histogram algorithm (Naive density estimator)

#The function takes three arguments: the evaluation points (x), the sample (X) and the bandwidth (h).
#The output is a grid chart with a comparison between a regular histogram, kde and the moved histogram, 
#all of them compared with an stardard normal.

moving_histogram <- function(x,X,h){

  require(ggplot2)
  require(gridExtra)
  n <- length(X)
  y <- c()
  f_hat <- c()
  for(j in 1:n){
    y <- sum((x[j] - h) < X & (x[j] + h) > X)
    f_hat[j] <- y/(2*n*h)
  }
  
  df <- data.frame(f_hat)
  df <- cbind(x, df, X)
  colnames(df) <- c("x", "y", "d")
  
  p1 <- ggplot(df) + geom_step(aes(x=x, y=y)) + 
    stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red", size = 2) +
    theme_bw() + xlim((min(X)), (max(X))) + ggtitle("Moving Histogram") +
    geom_rug(aes(x=df$d), sides="b")
  
  p2 <- ggplot(df) + stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red", size = 2) +
    theme_bw() + xlim((min(X)), (max(X))) + ggtitle("KDE") + 
    geom_density(aes(x=df$d), color = "blue") + 
    geom_rug(aes(x=df$d), sides="b")
  
  p3 <- ggplot(df,aes(x = d)) + geom_histogram(aes(y =..density..), fill = "white", color = "black") +
    stat_function(fun = dnorm, args = list(mean = mean(df$d), sd = sd(df$d)), color = "red", size = 2) +
    theme_bw() + xlim((min(X)), (max(X))) + ggtitle("Histogram") +
    geom_rug(aes(x=df$d), sides="b")
  
  
  grid <- grid.arrange(p3, p2, p1, 
               widths = c(1, 1),
               layout_matrix = rbind(c(1, 2), 3))
  return(grid)
}

sam = 5000
X <- rnorm(sam)
x <- seq(min(X),max(X), len = sam)
h <- 0.45

moving_histogram(x, X, h)
