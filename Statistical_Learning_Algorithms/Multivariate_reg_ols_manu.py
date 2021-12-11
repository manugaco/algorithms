
# Multivariate linear regression OLS estimator:

import numpy as np
import pandas as pd

# Functions to compute the mean squared error:

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

# Function to fit the OLS estimator on train data:

def f_ols_fit(df_train, target):

    # Shaping input:
    X = np.array(df_train.drop(target, axis=1))
    y = np.array(df_train[target])
    # Adding Bias (vector of 1's):
    X = np.concatenate((np.ones((X.shape[0], 1)), X), axis=1)
    # Estimating coeficients; b=(X'X)⁻1*X'y
    # Moore-Penrose pseudoinverse is used.
    coefs = np.dot(np.linalg.pinv(np.dot(X.T, X)), np.dot(X.T, y))
    fit = np.dot(X, coefs)

    return coefs, fit

# Function to predict y_hat on test data:

def f_ols_predict(df_test, coefs):

    # Shaping the input:
    X = np.array(df_test)
    X = np.concatenate((np.ones((X.shape[0], 1)), X), axis=1)

    # Return the y_hat values and the features in a dataframe:
    res = pd.concat([pd.DataFrame(np.dot(X, coefs)).rename(columns={0:'y_hat'}), df_test], axis=1)

    return res

# Testing the funtions with toy example (fit and predict):

X = np.array([[1, 2, 1], [2, 4, 1], [1, 1, 5]])
y = np.array([[1], [1], [2]])

df_train = pd.concat([pd.DataFrame(y).rename(columns={0:'target'}),
                      pd.DataFrame(X).rename(columns={0:'A', 
                                                      1:'B',
                                                      2:'C'})], axis=1)

coefs, y_pred = f_ols_fit(df_train, 'target')
pd.DataFrame(y, y_pred)
f_mse(y, y_pred)

X = np.array([[2, 3, 2], [3, 5, 2], [2, 2, 6]])
df_test = pd.DataFrame(X).rename(columns={0:'X1', 1:'X2', 2:'X3'})

df_train
f_ols_predict(df_test, coefs)

# Testing the functions with real world data (fit and predict):

url = 'https://raw.githubusercontent.com/manugaco/Algorithms/master/Datasets/qsar_fish_toxicity.csv'
data = pd.read_csv(url, sep=',')
data.head()
target = 'LC50_target'

# Train and test split:

sel = np.random.rand(len(data)) < 0.8
df_train = data[sel].reset_index(drop=True)
df_test = data[~sel].reset_index(drop=True)

# Fit:

coefs, y_pred = f_ols_fit(df_train, target)
pd.concat([df_train[target], pd.DataFrame(y_pred).rename(columns={0:'y_pred'})], axis=1).head()

# Predict:

result = f_ols_predict(df_test.drop(target, axis=1), coefs)
pd.concat([df_test[target], result['y_hat']], axis=1).head()

# Fit error:

f_mse(np.array(df_train[target].reset_index(drop=True)), y_pred)

# Prediction error:

f_mse(df_test[target], result['y_hat'])