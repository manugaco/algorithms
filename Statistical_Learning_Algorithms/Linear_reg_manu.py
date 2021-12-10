
## Libraries:

import numpy as np
import pandas as pd
from sklearn.datasets import load_breast_cancer

## Statistics:

# Mean:

def f_mean(x):
    return sum(x) / float(len(x))

# Sum of squares:

def f_SS(x):
    mean = f_mean(x)
    return sum([(i-mean)**2 for i in x])

# Covariance:

def f_cov(x, y):

    mean_x = f_mean(x)
    mean_y = f_mean(y)
    cov = []
    if len(x) != len(y):
        exit("Error, x and y must be of the same length")
    else:
        for i in range(len(x)):
            aux = (x[i] - mean_x) * (y[i] - mean_y)
            cov.append(aux)
        return sum(cov)

# Function to split a dataframe between features and target:

def f_stf(data, target):

    X = np.array(data.drop(target, axis=1))
    y = np.array(data[target])

    return(X, y)

# Simple linear regression coefficients:

def f_beta1(x, y):
    return(f_cov(x, y)/f_SS(x))

def f_beta0(x, y):
    return(f_mean(y) - (f_beta1(x, y)*f_mean(x)))

# Function to get the coefficients from training data:

def f_slr_fit(df_train, target):

    coef = []
    x_train, y_train = f_stf(df_train, target)
    coef.append(f_beta0(x_train, y_train)[0])
    coef.append(f_beta1(x_train, y_train)[0])

    return coef

# Function to predict the y values given test data:

def f_slr_predict(df_test, coef):
    
    preds = []
    x_test = np.array(df_test)
    b0 = coef[0]
    b1 = coef[1]

    for i in range(len(x_test)):

        y_hat = b0 + b1*x_test[i]
        preds.append(y_hat[0])

    return preds

# Function to get the mean squared error:

def f_mse(a, p):
    s_error = []

    if len(a) != len(p):
        exit("Error, actual and predicted values vectors must have same length.")
    else:
        for i in range(len(a)):
            s_error.append((a[i] - p[i])**2)

    return(f_mean(s_error))

# Function to get the root mean squared error:

def f_rmse(a, p):

    if len(a) != len(p):
        exit("Error, actual and predicted values vectors must have same length.")
    
    return f_mse(a, p)**(1/2)

# Function to get the mean absolute error:

def f_mae(a, p):

    s_error = []

    if len(a) != len(p):
        exit("Error, actual and predicted values vectors must have same length.")
    else:
        for i in range(len(a)):
            s_error.append(abs(a[i] - p[i]))

    return(f_mean(s_error))

# Testing functions:

x = [1, 2, 4, 3, 5]
y = [1, 3, 3, 2, 5]

df_train = pd.DataFrame(x, y).reset_index().rename(columns={'index':'y', 0:'x'})
df_train

x_test = [1, 2, 2, 1, 5]
y_test = [1, 3, 2, 1, 4]

df_test = pd.DataFrame(x_test).rename(columns={0:'x'})
df_test

f_mean(x)
f_mean(y)

f_SS(x)
f_SS(y)

f_cov(x, y)

f_beta1(x, y)
f_beta0(x, y)

coef = f_slr_fit(df_train, 'y')
coef
preds = f_slr_predict(df_test, coef)
preds

f_mse(y_test, preds)
f_rmse(y_test, preds)
f_mae(y_test, preds)
