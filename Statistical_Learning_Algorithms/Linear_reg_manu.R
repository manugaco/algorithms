
f_mean <- function(x){
    return(sum(x)/length(x))
}

f_SS <- function(x){
    aux <- c()
    for(i in 1:length(x)){
        mean <- f_mean(x)
        aux[i] = (x[i] - mean)**2
    }
    return(sum(aux))
}

f_cov <- function(x, y){
    mean_x <- f_mean(x)
    mean_y <- f_mean(y)
    cov <- c()

    if(length(x)!=length(y)){
        print("Error, x and y must be the same length")
    }else{
        for(i in 1:length(x)){
            cov[i] <- ((x[i] - mean_x)*( y[i] - mean_y))
        }
    }
    return(sum(cov))
}

f_beta1 <- function(x, y){
    return(f_cov(x, y)/f_SS(x))
}

f_beta0 <- function(x, y){
    return(f_mean(y) - (f_beta1(x, y)*f_mean(x)))
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
    return(f_mean(s_error))
}

f_rmse <- function(a, p){
    if(length(a)!=length(p)){
        print("Error, actual and predicted values vectors must have same length.")
    }

    return(f_mse(a, p)**(1/2))
}

f_simple_linear_regression <- function(train, test){

    preds <- c()

    b1 <- f_beta1(x, y)
    b0 <- f_beta0(x, y)


}

# Testing functions:

x <- c(1, 2, 4, 3, 5)
y <- c(1, 3, 3, 2, 5)

f_mean(x)
f_mean(y)

f_SS(x)
f_SS(y)

f_cov(x, y)

f_beta1(x, y)

f_beta0(x, y)

# Predictions:


