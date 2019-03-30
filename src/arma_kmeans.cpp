// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;                      

// [[Rcpp::export]]
List arma_kmeans(arma::mat x, int k) {
  
  mat means;
  kmeans(means, x, k, random_subset, 10, false);

  return List::create(_["k"]	= k, 
                      _["cluster"] = means);
}

