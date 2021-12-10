import numpy as np




def f_mean(x):
    return sum(x) / float(len(x))

def f_SS(x):
    mean = f_mean(x)
    return sum([(i-mean)**2 for i in x])

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

def f_beta1(x, y):
    return(f_cov(x, y)/f_SS(x))

def f_beta0(x, y):
    return(f_mean(y) - (f_beta1(x, y)*f_mean(x)))

def f_mse(a, p):
    s_error = []

    if len(a) != len(p):
        exit("Error, actual and predicted values vectors must have same length.")
    else:
        for i in range(len(a)):
            s_error.append((a[i] - p[i])**2)

    return(f_mean(s_error))

def f_rmse(a, p):

    if len(a) != len(p):
        exit("Error, actual and predicted values vectors must have same length.")
    
    return f_mse(a, p)**(1/2)

def f_simple_linear_regression(train, test):
    
    preds = []

    b1 = f_beta1(x, y)
    b0 = f_beta0(x, y)


# Testing functions:

dataset = [[1, 1], [2, 3], [4, 3], [3, 2], [5, 5]]
x = [row[0] for row in dataset]
y = [row[1] for row in dataset]

f_mean(x)
f_mean(y)

f_SS(x)
f_SS(y)

f_cov(x, y)

f_beta1(x, y)

f_beta0(x, y)

# Predictions:


