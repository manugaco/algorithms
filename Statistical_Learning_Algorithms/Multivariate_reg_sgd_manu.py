import numpy as np
import pandas as pd

def predict(row, coefficients):
    yhat = coefficients[0]
    for i in range(len(row)-1):
        yhat += coefficients[i + 1] * row[i]
    return yhat

def coefficients_sgd(train, l_rate, n_epoch):
    coef = [0.0 for i in range(train.shape[1])]
    for epoch in range(n_epoch):
        sum_error = 0
        for row in train:
            yhat = predict(row, coef)
            error = yhat - row[-1]
            sum_error += error**2
            coef[0] = coef[0] - l_rate * error
            for i in range(len(row)-1):
                coef[i + 1] = coef[i + 1] - l_rate * error * row[i]
    print( 'epoch=%d, lrate=%.3f, error=%.3f ' % (epoch, l_rate, sum_error))
    return coef

dataset = pd.DataFrame([[1, 1], [2, 3], [4, 3], [3, 2], [5, 5]])
l_rate = 0.001
n_epoch = 50
coef = coefficients_sgd(dataset, l_rate, n_epoch)
print(coef)

coef = [0.0 for i in range(dataset.shape[1])]

for row in dataset:
    print(row)