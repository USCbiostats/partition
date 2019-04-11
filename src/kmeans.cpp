// #include "RcppMLPACK.h"
//
// using namespace mlpack::kmeans;
// using namespace Rcpp;
//
// // [[Rcpp::depends(RcppMLPACK)]]
// // [[Rcpp::export]]
// List kmeans_c(SEXP x, const int& clusters) {
//
//   NumericMatrix Xr(x);
//   arma::mat X(Xr.begin(), Xr.nrow(), Xr.ncol(), false);
//   arma::Col<size_t> assignments;
//
//   // Initialize with the default arguments.
//   KMeans<> k;
//
//   k.Cluster(X, clusters, assignments);
//
//   return List::create(_["k"]	= clusters,
//                       _["cluster"]		= assignments);
// }

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

void set_seed(int seed) {

  Rcpp::Environment base_env("package:base");

  Rcpp::Function set_seed_r = base_env["set.seed"];

  set_seed_r(seed);
}

// [[Rcpp::export]]
arma::mat kmeans_c(arma::mat& x, int k, int n_iter = 10, bool verbose = false, int seed = 1) {
  set_seed(seed);

  mat means;
  bool status;

  status = kmeans(means, x.t(), k, arma::random_subset, n_iter, verbose);

  return means.t();
}

// code from ClusteR

int minimum_cluster(arma::vec x) {

  double out = arma::datum::inf;
  int idx = 0;

  for (int i = 0; i < x.n_elem; i++) {
    if (x(i) < out) {
      out = x(i);
      idx = i + 1;
    }
  }

  return(idx);
}

arma::vec wcss(arma::rowvec vec, arma::mat centroids) {
  arma::vec row_wcss(centroids.n_rows);

  for (int i = 0; i < centroids.n_rows; i++) {
    row_wcss(i) = arma::as_scalar(arma::accu(arma::pow(vec - centroids.row(i), 2)));
  }

  return row_wcss;
}

// [[Rcpp::export]]
NumericVector assign_cluster(arma::mat& x, arma::mat init_centroids) {

  arma::vec assignments(x.n_rows);

  for (int k = 0; k < x.n_rows; k++) {
    arma::vec wcss_vec = wcss(arma::conv_to< arma::rowvec >::from(x.row(k)), init_centroids);
    assignments(k) = minimum_cluster(wcss_vec);
  }

  Rcpp::NumericVector assignments_nv = Rcpp::wrap(assignments);
  assignments_nv.attr("dim") = R_NilValue;
  return assignments_nv;
}

// [[Rcpp::export]]
NumericVector kmean_assignment(arma::mat& x, int k, int n_iter = 10, bool verbose = false, int seed = 1) {
  NumericVector assignments(x.n_cols);
  arma::mat centroids;
  arma::mat transposed_data = x.t();

  centroids = kmeans_c(transposed_data, k, n_iter, verbose, seed);
  assignments = assign_cluster(transposed_data, centroids);
  return assignments;
}
