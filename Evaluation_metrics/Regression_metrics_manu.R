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
    return(f_mean(s_error))
}

f_rmse <- function(a, p){
    if(length(a)!=length(p)){
        print("Error, actual and predicted values vectors must have same length.")
    }

    return(f_mse(a, p)**(1/2))
}

f_mae <- function(a,p){
    s_error <- c()

    if(length(a)!=length(p)){
        print("Error, actual and predicted values vectors must have same length.")
    }else{
        for(i in 1:length(a)){
            s_error[i] <- abs(a[i] - p[i])
        }

    }
    return(f_mean(s_error))
}

# Testing functions:

expected <- c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
predicted <- c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0)

f_mse(expected, predicted)
f_rmse(expected, predicted)
f_mae(expected, predicted)
