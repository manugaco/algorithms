
# Implementation of the PCA algorithm from scratch:

import numpy as np



def PCA(X, n_comp):
    
    # Arguments:
    # X = Input matrix.
    # n_comp = number of components.

    # Computing mean vector, the covariance matrix, eigenvalues and eigenvectors
    means = X - np.mean(X, axis=0)
    eig_val, eig_vec = np.linalg.eigh(np.cov(means, rowvar=False))

    # Sorting eigenvalues, eigenvectors and get the number of desired components:
    ind_exp = []
    for i in range(len(sorted(eig_val)[::-1])):
        ind_exp.append((len(sorted(eig_val)[::-1]) - i)-1)
    sub = eig_vec[:,ind_exp][:,0:n_comp]

    # Returning the reduced input matrix: 
    return np.dot(sub.transpose(), means.transpose()).transpose()

# Dummy data:

X = np.random.randint(20, 40, 500).reshape(50, 10)

PCA(X, 2)

