#include "RcppMLPACK.h"

using namespace mlpack::kmeans;
using namespace Rcpp;                                  

// [[Rcpp::depends(RcppMLPACK)]]
// [[Rcpp::export]]
List kmeans_c(SEXP data, const int& clusters) {

  NumericMatrix Xr(data);
  arma::mat X(Xr.begin(), Xr.nrow(), Xr.ncol(), false); 
  arma::Col<size_t> assignments;

  // Initialize with the default arguments.
  KMeans<> k;

  k.Cluster(X, clusters, assignments); 

  return List::create(_["k"]	= clusters,
                      _["cluster"]		= assignments);
}