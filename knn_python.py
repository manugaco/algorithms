
def knn_python(X, X0, Y):
  
  nrow = len(X)
  ncol = len(X[0])
  
  mind = 9999999999
  minc = -1
  
  for i in range(0, nrow):
    dist = 0
    for j in range(0, ncol):
      
      diff = X[i,j] - X0[j]
      dist = dist + diff * diff

    if dist < mind:
      mind = dist
      minc = Y[i]
  return(minc)
