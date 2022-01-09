# K-means implementation in Python.
# - The input of the function takes the following arguments:
#     - Features dataframe.
#     - Number of centroids "k".
#     - Number of iterations to compute, (default 100).
#     - Type of distance (euclidean, squared euclidean or manhattan).
# - The output of the function is:
#     - Dataframe with original data and the associated centroid.

#Libraries

import pandas as pd
import numpy as np
from random import sample
from sklearn.datasets import load_iris

#Function:

def kmeans_manu(data, k, niter = 50, dist = "euclidean"):
    
    #k value sanity check:
    if (k < 2):
        exit('Number of clusters must be higher than 1')

    ncol = data.shape[1]
    nrow = data.shape[0]
    
    #Initial centroids:
    
    cent_ls = []
    cent = np.zeros(shape = (k, ncol))

    for i in range(0, ncol):
        cent[:,i] = sample(tuple(data[:,i]), k)
    cent_ls.append(cent)
    
    #Distances from observations to centroids
    dist_ls = []
    clus = []
    dist = np.zeros(shape=(nrow, k))

    for i in range(0, (nrow)):
        for j in range(0, (ncol-1)):

            #Distance metric:
            if dist == "Euclidean":
                dist[i, j] = np.sqrt((sum(data.iloc[i, :] - cent.iloc[j, :]))**2)
            if dist == "Euclidean_sq":
                dist[i, j] = (sum(data.iloc[i, :] - cent.iloc[j, :]))**2
            if dist == "Manhattan":
                dist[i, j] = sum(abs(data.iloc[i, :] - cent.iloc[j, :]))

        clus.append(np.argmin(dist[i, :]))
        
    dist_ls.append(dist)
    data_df = pd.DataFrame(data)
    clus_df = pd.DataFrame(clus)
    data_c = pd.concat([data_df.reset_index(drop = True), clus_df], axis = 1)

    iter = 0
    while iter < niter:
    
        #Recompute centroids
        cent_ls = []
        cent = np.zeros(shape=(k, ncol))
        for i in range(0, ncol):
            cent[:, i] = sample(tuple(data.iloc[:, i]), k)
        cent_ls.append(cent)
        
        #Recompute distances
        
        dist = np.zeros(shape=(nrow, k))
        clus = []
        for i in range(0, (nrow)):
            for j in range(0, (ncol-1)):

                #Distance metric:
                if dist == "euclidean":
                    dist[i, j] = np.sqrt((sum(data.iloc[i, :] - cent.iloc[j, :]))**2)
                if dist == "euclidean squared":
                    dist[i, j] = (sum(data.iloc[i, :] - cent.iloc[j, :]))**2
                if dist == "manhattan":
                    dist[i, j] = sum(abs(data.iloc[i, :] - cent.iloc[j, :]))
            clus.append(np.argmin(dist[i,:]))

        dist_ls.append(dist)
        data_df = pd.DataFrame(data)
        clus_df = pd.DataFrame(clus)
        data_c = pd.concat([data_df.reset_index(drop=True), clus_df], axis=1)

        iter = iter + 1
        
    return(data_c)

    

#Example

data = load_iris()
df_features = pd.DataFrame(data.data, columns = data.feature_names)
kmeans_manu(df_features, k = 4, niter = 50, dist = "manhattan")
