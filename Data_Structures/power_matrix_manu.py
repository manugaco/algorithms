# Description: Simple function to compute the power of a matrix in R programming.

import numpy as np

def matpow(m, p):

    #Arguments:

    # m = squared matrix.
    # p = desired power to rise the matrix.

    # Sanity check: Matrix must be squared.

    if m.shape[0] != m.shape[1]:
        exit("Error: Matrix is not squared")

    # Function body:



mat = np.array([[1, 2, 1], [1, 3, 4], [1, 2, 3]])

matpow(mat, 2)


p=3
p=np.array([p])

