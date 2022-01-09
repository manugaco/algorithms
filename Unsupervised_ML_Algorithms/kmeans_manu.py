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
from sklearn.datasets import load_iris

#k-means Function:

def kmeans_manu(data, k, niter = 50, metric = "euclidean"):
    
    #k value sanity check:
    if (k < 2):
        exit('Number of clusters must be higher than 1')

    #Algorithm body:
    #Initial centroids:
    cent = []
    for i in range(0, k):
        cent_coord = []
        for j in range(0, data.shape[1]):
            cent_coord.append(np.random.uniform(data.iloc[:,j].min(), data.iloc[:,j].max()))
        cent.append(cent_coord)
    cent = np.array(cent)

    #Distances from observations to centroids:
    iter = 0
    while iter < niter:
        dist_ls = []

        #Compute distances between centroids and datapoints:
        for i in range(data.shape[0]):
            dist = []
            for j in range(len(cent)):

                #Distance metric:
                if metric == "euclidean":
                    dist.append(np.sqrt((sum(np.array(data.iloc[i,:]) - cent[j]))**2))
                if metric == "euclidean squared":
                    dist.append(sum(np.array(data.iloc[i,:]) - cent[j])**2)
                if metric == "manhattan":
                    dist.append(sum(np.array(data.iloc[i,:]) - cent[j]))
            dist_ls.append(dist)

        df_dist = pd.DataFrame(dist_ls)
        data_c = pd.concat([data, 
                            df_dist.idxmin(axis=1)], 
                            axis=1).rename(columns={0:'Clusters'})
        
        #Recompute centroids by mean:
        cent = np.array(data_c.groupby('Clusters').mean())
        iter = iter+1

    return(data_c)

#Iris Dataset Example:

iris = load_iris()
data = pd.DataFrame(iris.data, columns = iris.feature_names)
results = kmeans_manu(data, k = 3, niter = 10, metric = "euclidean")
pd.crosstab(np.array(results['Clusters']), iris.target)