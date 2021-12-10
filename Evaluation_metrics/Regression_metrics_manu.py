def f_mean(x):
    return sum(x) / float(len(x))

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

def f_mae(a, p):

    s_error = []

    if len(a) != len(p):
        exit("Error, actual and predicted values vectors must have same length.")
    else:
        for i in range(len(a)):
            s_error.append(abs(a[i] - p[i]))

    return(f_mean(s_error))

# Testing functions:

expected = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
predicted = [1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0]

f_mse(expected, predicted)
f_rmse(expected, predicted)
f_mae(expected, predicted)