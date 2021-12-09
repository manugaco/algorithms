# K-means implementation in Python (v3.6).
#
# - First, I have made a function to generate data (normaly distributed)
# - Then I have implemented the k-means algorithm in a function, with three three different distances.
# - The input of the function takes the following arguments:
#     - Matrix with the variables.
#     - Number of centroids "k".
#     - Number of iterations, (default 100).
#     - Type of distance (euclidean, squared euclidean and manhattan).
#     - If The function should plot each combination (FALSE by default.
# - The output of the function is:
#     - List of Pandas Dataframes with the original data and a column with the corresponding centroid, for each iteration.
# - Required libraries (pandas, numpy, seaborn, random)

#Libraries

import pandas as pd
import numpy as np
import seaborn as sns
import random
import traceback
from random import sample
import math

#Function

def kmeans_manu(data, k, niter = 50, dist = "euclidean", plot_PCA = False):
     
     col = data.shape[1]
     nrow = data.shape[0]
    
    if dist == "euclidean":
        
        #Initial centroids
        
        cent_ls = []
        cent = np.zeros(shape=(k, ncol))

        for i in range(0, ncol):
            cent[:,i] = sample(tuple(data[:,i]), k)

        cent_ls.append(cent)
        
        #Distances from observations to centroids
        
        dist_ls = []
        clus = []
        dist = np.zeros(shape=(nrow, k))

        for i in range(0, (nrow)):
            for j in range(0, (ncol-1)):
                dist[i,j] = math.sqrt((sum(data[i,:] - cent[j,:]))**2)
            clus.append(np.argmin(dist[i,:]))
            
        dist_ls.append(dist)
        data_df = pd.DataFrame(data)
        clus_df = pd.DataFrame(clus)
        data_c = pd.concat([data_df.reset_index(drop=True), clus_df], axis=1)
        data_clus = data_c.values

        for h in range(1, niter):
        
            #Recompute centroids
            
            cent = np.zeros(shape=(k, ncol))
            for i in range(0, ncol):
                cent[:,i] = sample(tuple(data[:,i]), k)

            cent_ls.append(cent)
            
            #Recompute distances
            
            dist = np.zeros(shape=(nrow, k))
            clus = []
            for i in range(0, (nrow)):
                for j in range(0, (ncol-1)):
                    dist[i,j] = math.sqrt((sum(data[i,:] - cent[j,:]))**2)
                clus.append(np.argmin(dist[i,:]))

            dist_ls.append(dist)
            data_df = pd.DataFrame(data)
            clus_df = pd.DataFrame(clus)
            data_c = pd.concat([data_df.reset_index(drop=True), clus_df], axis=1)
            
        return(cent_ls, data_c)

    if dist == "euclidean squared":
        
        #Initial centroids
        
        cent_ls = []
        cent = np.zeros(shape=(k, ncol))

        for i in range(0, ncol):
            cent[:,i] = sample(tuple(data[:,i]), k)

        cent_ls.append(cent)
        
        #Distances from observations to centroids
        
        dist_ls = []
        clus = []
        dist = np.zeros(shape=(nrow, k))

        for i in range(0, (nrow)):
            for j in range(0, (ncol-1)):
                dist[i,j] = (sum(data[i,:] - cent[j,:]))**2
            clus.append(np.argmin(dist[i,:]))
            
        dist_ls.append(dist)
        data_df = pd.DataFrame(data)
        clus_df = pd.DataFrame(clus)
        data_c = pd.concat([data_df.reset_index(drop=True), clus_df], axis=1)
        data_clus = data_c.values

        for h in range(1, niter):
            #Recompute centroids
            cent = np.zeros(shape=(k, ncol))
            for i in range(0, ncol):
                cent[:,i] = sample(tuple(data[:,i]), k)

            cent_ls.append(cent)
            #Recompute distances
            dist = np.zeros(shape=(nrow, k))
            clus = []
            for i in range(0, (nrow)):
                for j in range(0, (ncol-1)):
                    dist[i,j] = (sum(data[i,:] - cent[j,:]))**2
                clus.append(np.argmin(dist[i,:]))

            dist_ls.append(dist)
            data_df = pd.DataFrame(data)
            clus_df = pd.DataFrame(clus)
            data_c = pd.concat([data_df.reset_index(drop=True), clus_df], axis=1)
            
        return(cent_ls, data_c)
        
    if dist == "manhattan":
        
        #Initial centroids
        
        cent_ls = []
        cent = np.zeros(shape=(k, ncol))

        for i in range(0, ncol):
            cent[:,i] = sample(tuple(data[:,i]), k)

        cent_ls.append(cent)
        
        #Distances from observations to centroids
        
        dist_ls = []
        clus = []
        dist = np.zeros(shape=(nrow, k))

        for i in range(0, (nrow)):
            for j in range(0, (ncol-1)):
                dist[i,j] = sum(abs(data[i,:] - cent[j,:]))
            clus.append(np.argmin(dist[i,:]))
            
        dist_ls.append(dist)
        data_df = pd.DataFrame(data)
        clus_df = pd.DataFrame(clus)
        data_c = pd.concat([data_df.reset_index(drop=True), clus_df], axis=1)
        data_clus = data_c.values

        for h in range(1, niter):
            #Recompute centroids
            cent = np.zeros(shape=(k, ncol))
            for i in range(0, ncol):
                cent[:,i] = sample(tuple(data[:,i]), k)

            cent_ls.append(cent)
            #Recompute distances
            dist = np.zeros(shape=(nrow, k))
            clus = []
            for i in range(0, (nrow)):
                for j in range(0, (ncol-1)):
                    dist[i,j] = sum(abs(data[i,:] - cent[j,:]))
                clus.append(np.argmin(dist[i,:]))

            dist_ls.append(dist)
            data_df = pd.DataFrame(data)
            clus_df = pd.DataFrame(clus)
            data_c = pd.concat([data_df.reset_index(drop=True), clus_df], axis=1)
            
        return(cent_ls, data_c)

#Example

k=4

kmeans_manu(data, k, niter = 50, dist = "manhattan")
