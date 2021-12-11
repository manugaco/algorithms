
# Multivariate linear regression with OLS estimator:

library(MASS)

# Functions to compute the mean squared error:

f_mean <- function(x){
    return(sum(x)/length(x))
}

f_mse <- function(a,p){
    s_error <- c()

    if(length(a)!=length(p)){
        print("Error, actual and predicted values vectors must have same length.")
    }else{
        for(i in 1:length(a)){
            s_error[i] <- (a[i] - p[i])**2
        }
    }
    return(round(f_mean(s_error), 4))   
}
# Function to compute the OLS estimator:

f_ols_fit <- function(df_train, target){

    # Shaping the input:
    X <- as.matrix(df_train[, names(df_train) != target], ncol=ncol(df_train)-1)
    y <- as.matrix(df_train[, names(df_train) == target], ncol=1)

    # Adding Bias (vector of 1's):
    v_ones <- c()
    for(i in 1:nrow(X))
        v_ones[i] <- 1
    X <- cbind(matrix(v_ones, ncol=1), X)
    rownames(X) <- NULL
    colnames(X) <- NULL

    # Estimating coeficients; b=(X'X)⁻1*X'y
    # Moore-Penrose pseudoinverse is used.
    coefs <- (ginv(t(X)%*%X))%*%(t(X)%*%y)
    fit <- X%*%coefs

    return(list(coefs, fit))
}

# Function to predict y_hat on test data:

f_ols_predict <- function(df_test, coefs){

    # Shaping the input:
    X <- as.matrix(df_test, ncol=ncol(df_test))
    v_ones <- c()
    for(i in 1:nrow(X))
        v_ones[i] <- 1
    X <- cbind(matrix(v_ones, ncol=1), X)
    rownames(X) <- NULL
    colnames(X) <- NULL

    # Return the y_hat values and the features in a matrix:
    res <- cbind(X%*%coefs, X[,2:(ncol(X))])
    colnames(res) <- c(c('y_hat'), colnames(df_test))
    return(res)
}

# Testing the functions with toy example (fit and predict):

x <- matrix(c(1, 2, 1, 2, 4, 1, 1, 1, 5), ncol=3, nrow=3, byrow=T)
y <- matrix(c(1, 1, 2), ncol = 1)
df_train <- data.frame(y, x)
names(df_train) <- c('target', 'A', 'B', 'C')

fit_res <- f_ols_fit(df_train, 'target')

coef_fit <- fit_res[[1]]
y_pred <- fit_res[[2]]
data.frame(y, y_pred)
f_mse(y, y_pred)

x <- matrix(c(2, 3, 2, 3, 5, 2, 2, 2, 6), ncol=3, nrow=3, byrow=T)
df_test <- data.frame(x)

f_ols_predict(df_test, coef_fit)