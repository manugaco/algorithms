## Neural Network from scratch in R programming language:

# Testing dataset:

X <- matrix(c(0, 1, 0, 0, 0, 0, 1, 1,
              0, 1, 0, 1), nrow=3, ncol=4)

y <- matrix(c(1, 1, 0), byrow=FALSE)


# Activation functions:

sigmoid <- function(x){
    1/(1+exp(-x))
}

d_sigmoid <- function(x){
    sigmoid(x)*(1-sigmoid(x))
}

# Initializing variables:

ep = 5000
alpha = 0.1
iln = ncol(X)
hln = 3
oln = 1

losses <- c()

wih <- matrix(runif(iln*hln), iln, hln)
#bih <- matrix(runif())
woh <- matrix(runif(hln*oln), hln, oln)

bias_in=runif(hln)
bias_in_temp=rep(bias_in, nrow(X))
bh=matrix(bias_in_temp, nrow = nrow(X), byrow = FALSE)

bias_out=runif(oln)
bias_out_temp=rep(bias_out,nrow(X))
bout=matrix(bias_out_temp,nrow = nrow(X),byrow = FALSE)