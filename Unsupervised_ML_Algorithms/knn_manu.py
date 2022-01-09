
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split

## Knn function:

def knn_manu(X_train, y_train, X_test, k='Auto'):

  # Args: 
  # X_train = Training features set.
  # y_train = Training target labels.
  # X_test = Test Features.
  # k = Number of Neightbours.

  # k automatic selection:
  if (k=='Auto'):
    k = round(np.sqrt(X_train.shape[0]+X_test.shape[0]))

  # Odd number of neightbours:
  if (k % 2 == 0):
    k = k+1

  #Formating inputs:
  X = np.array(X_train)
  X0 = np.array(X_test)

  #Algorithm body:
  distmat = []
  for i in range(len(X_test)):
    dist = []
    for j in range(len(X_train)):
      dist.append(np.sqrt(sum(X0[i]-X[j])**2))
    distmat.append(dist)

  #Distances dataframe:
  df_dist = pd.DataFrame(distmat)
  df_dist.columns = X_train.index
  df_dist = df_dist.set_index(X_test.index)

  #Computing the new labels (y_hat):
  newlabs = []
  for i in range(len(X_test)):
    keep_ind = np.array(df_dist.iloc[i,].sort_values()[0:k,].index)
    newlabs.append(y_train.loc[X_train.index.isin(keep_ind)].value_counts().idxmax())

  #Output: New labels.
  return newlabs

#Iris dataset example:

data = load_iris()
df_features = pd.DataFrame(data.data, columns = data.feature_names)
df_target = pd.DataFrame(data.target, columns = ['Target'])
df_iris = pd.concat([df_target, df_features], axis=1)
df_iris['Target'] = np.where(df_iris['Target'] == 0, 'setosa',
                    np.where(df_iris['Target'] == 1, 'versicolor', 'virginica'))

X_train, X_test, y_train, y_test = train_test_split(df_iris.drop('Target', axis=1), 
                                                    df_iris['Target'], 
                                                    test_size=0.2, 
                                                    random_state=123)

y_hat = knn_manu(X_train, y_train, X_test, k=6)
pd.crosstab(np.array(y_test), np.array(y_hat))