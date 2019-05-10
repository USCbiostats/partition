// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
double icc_c(const NumericMatrix& x) {
  NumericVector icc_vec, ms1, ms2, variance;
  double icc;
  int ncols = x.ncol(), nrows = x.nrow();
  NumericVector row_vec(ncols), rowmeans(nrows), long_means(nrows * ncols), long_mat(nrows * ncols),
    among, within;

  double matrix_mean = mean(na_omit(x));

  rowmeans = rowMeans(x, true);
  long_means = rep(rowmeans, ncols);
  long_mat = as<NumericVector>(x);

  within = pow(long_mat - long_means, 2);
  among = pow(long_means - matrix_mean, 2);

  ms1 = sum(na_omit(among)) / (nrows - 1);
  ms2 = sum(na_omit(within)) / (nrows * (ncols - 1));

  variance = (ms1 - ms2) / ncols;

  icc_vec = variance / (variance + ms2);
  icc = icc_vec[0];

  return icc;
}

// [[Rcpp::export]]
NumericVector scale_rowmeans(NumericMatrix x) {

  NumericVector row_vec(x.ncol()), out(x.nrow());

  for(int i = 0; i < x.nrow(); ++i) {
    row_vec = x(i, _);
    out[i] = mean(na_omit(row_vec));
  }

  return (out - mean(out)) / sd(out);
}

// [[Rcpp::export]]
NumericMatrix subset_matrix(List& x, int& i, arma::mat& m) {
  IntegerVector y = x[i];
  arma::uvec y_u = as<arma::uvec>(y);
  arma::mat m2 = m.cols(y_u);
  return wrap(m2);
}

// [[Rcpp::export]]
NumericVector min_icc_c(List& columns, arma::mat& x, int& k, double& threshold) {

  NumericMatrix x_subset;
  NumericVector icc_subset(k);

  for (int i = 0; i < k; ++i) {
    x_subset = subset_matrix(columns, i, x);
    if (x_subset.ncol() == 1) {
      // only one variable so no information loss
      icc_subset[i] = 1;
    } else {
      icc_subset[i] = icc_c(x_subset);
      if (icc_subset[i] <= threshold) break;
    }
  }

  return(icc_subset);
}
