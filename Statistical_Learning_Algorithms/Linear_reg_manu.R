
## Statistics:

# Mean:

f_mean <- function(x){
    return(sum(x)/length(x))
}

# Sum of squares:

f_SS <- function(x){
    aux <- c()
    for(i in 1:length(x)){
        mean <- f_mean(x)
        aux[i] = (x[i] - mean)**2
    }
    return(sum(aux))
}

# Covariance:

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

# Function to split a dataframe between features and target:

f_stf <- function(data, target){

    X <- c(data[, names(data) != target])
    y <- c(data[, names(data) == target])

    return(list(X, y))
}

# Simple linear regression coefficients:

f_beta1 <- function(x, y){
    return(f_cov(x, y)/f_SS(x))
}

f_beta0 <- function(x, y){
    return(f_mean(y) - (f_beta1(x, y)*f_mean(x)))
}

# Function to get the coefficients from training data:

f_srl_fit <- function(df_train, target){

    coef <- c()
    ft_list = f_stf(df_train, target)
    x_train <- ft_list[[1]]
    y_train <- ft_list[[2]]
    b1 <- f_beta1(x_train, y_train)
    coef <- append(b1, f_beta0(x_train, y_train))
    
    return(coef)
}

# Function to predict the y values given test data:



# Function to get the mean squared error:

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

# Function to get the root mean squared error:

f_rmse <- function(a, p){
    if(length(a)!=length(p)){
        print("Error, actual and predicted values vectors must have same length.")
    }

    return(f_mse(a, p)**(1/2))
}

# Function to get the mean absolute error:

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

x <- c(1, 2, 4, 3, 5)
y <- c(1, 3, 3, 2, 5)

df_train = data.frame(y, x)
df_train

x_test <- c(1, 2, 2, 1, 5)
y_test <- c(1, 3, 2, 1, 4)

df_test <- data.frame()

f_mean(x)
f_mean(y)

f_SS(x)
f_SS(y)

f_cov(x, y)

f_beta1(x, y)
f_beta0(x, y)

coef <- f_srl_fit(df_train, 'y')
coef
preds <- f_slr_predict(df_test, coef)
preds

f_mse(y_test, preds)
f_rmse(y_test, preds)
f_mae(y_test, preds)
