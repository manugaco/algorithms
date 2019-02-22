#This is the implementation of the metropolis algorithm in a logistic regression hierarquical problem with bayesian approach.

#This is the latex code of the hierarquical model:

$$Y \mid X, \theta \sim Bern(\theta)$$
$$logit(\theta) = log\left(\frac{\theta}{1-\theta}\right) = \alpha + \beta \ x$$
$$\alpha \sim N(0,5)$$
$$\beta \sim N(0,2)$$

#Defining the data
y = c(1, 0, 1, 1, 0, 0)
x = c(3, 5, 2, 5, 10, 6)
#First I set up the range of the simulation

burn.in <- 1000
N <- 10000 + burn.in
alpha=beta=rep(NA,N)

#Then I set the initial values of alpha and beta:

h <- length(y)
set.seed(123)

#This is the function to compute the posterior probability of alpha and beta

post <- function(alpha, beta){
  theta <- c()
  for(i in 1:h){
    link <- (exp(alpha + beta*x[i])/(1 + exp(alpha + beta*x[i])))
    theta[i] <- dbinom(y[i], 1, link, log = TRUE)
  }
  sum(theta) + dnorm(alpha, 0, 5, log = TRUE) + dnorm(beta, 0, 2, log = TRUE)
}
#This is the metropolis algorithm implementation
#I set the starting points of the parameters

alpha[1] <- 5
beta[1] <- 2

for(i in 2:N){
  alpha.star <- rnorm(1, alpha[i-1], 5)
  beta.star <- rnorm(1, beta[i-1], 2)
  a <- post(alpha.star, beta.star) - post(alpha[i-1], beta[i-1])
  if(log(runif(1)) < a){
    alpha[i] <- alpha.star
    beta[i] <- beta.star}else{
      alpha[i] <- alpha[i-1]
      beta[i] <- beta[i-1]
    }
  }
  
#Parameters
library(kableExtra)
library(tidyverse)

mean <- rbind(mean(alpha), mean(beta))
var <- rbind(var(alpha), var(beta))
df <- cbind(mean, var)
rownames(df) <- c("Alpha", "Beta")
colnames(df) <- c("Mean", "Variance")
kable(df) %>%
  kable_styling(position = "center", bootstrap_options = "striped", full_width = F)
  
#Plot the results

par(mfrow=c(2,3))
plot.ts(alpha)
acf(alpha)
plot(density(alpha))
plot.ts(beta)
acf(beta)
plot(density(beta))
  
 

  
