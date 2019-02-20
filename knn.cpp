#include <Rcpp.h>
#include <iostream>
#include <limits>

using namespace Rcpp;


// My knn function

// [[Rcpp::export]]

   int my_knn_c(NumericMatrix X, NumericVector X0, NumericVector Y){
      int i, j, minclass;
      double dist, diff, mind;
      int ncol = X.ncol();
      int nrow = X.nrow();
      
      mind = 99999999999;
      minclass = -1;
    
      for(i = 0; i < nrow; ++i){
        
        dist = 0;
  
        for(j = 0; j < ncol; ++j){
          
          diff = X(i,j) - X0[j];
          dist = dist + (diff * diff);
          
        }
        
      if(dist < mind){
        mind = dist;
        minclass = Y[i];
        }
      
      }
      
      return(minclass);
}

   // [[Rcpp::export]]
   
   // Robust version
   
   int my_knn_c2(NumericMatrix X, NumericVector X0, NumericVector Y){
     int i, j, nx, ny, minclass;
     double dist, diff, mind;
     int ncol = X.ncol();
     int nrow = X.nrow();
     nx = X0.size();
     ny = Y.size();
     
     if(ncol != nx){
       std::cout << "Error: Number of columns on X must be equal to length of X0";
     }
     
     if(ncol != nx){
       std::cout << "Error: Number of columns on X must be equal to length of Y";
     }
     
     mind = std::numeric_limits<double>::max();
     minclass = -1;
     
     for(i = 0; i < nrow; ++i){
       
       dist = 0;
       
       for(j = 0; j < ncol; ++j){
         
         diff = X(i,j) - X0[j];
         dist = dist + (diff * diff);
         
       }
       
       if(dist < mind){
         mind = dist;
         minclass = Y[i];
       }
       
     }
     
     return(minclass);
   }








