#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector icc_c(NumericMatrix x) {
  NumericVector icc, ms1, ms2, variance;
  int ncols = x.ncol(), nrows = x.nrow();
  NumericVector row_vec(ncols), rowmeans(nrows), long_means(nrows * ncols), 
    among, within;
  
  double matrix_mean = mean(x);
  
  for (int i = 0; i < nrows; ++i) {
    row_vec = x(i, _);
    rowmeans[i] = mean(row_vec);
    row_vec = row_vec - rowmeans[i];
    for (NumericVector::iterator it = row_vec.begin(); it != row_vec.end(); ++it) {
      within.insert(within.end(), *it);
    }
  }
  
  long_means = rep(rowmeans, ncols);
  among = pow(long_means - matrix_mean, 2);
  within = pow(within, 2);
  ms1 = sum(among) / (nrows - 1); 
  ms2 = sum(within) / (nrows * (ncols - 1));
  variance = (ms1 - ms2) / ncols;
  icc = variance / (variance + ms2);
  
  return icc;
}

// [[Rcpp::export]]
NumericVector scale_rowmeans(NumericMatrix x) {
  
  NumericVector row_vec(x.ncol()), out(x.nrow());
  
  for(int i = 0; i < x.nrow(); ++i) {
    row_vec = x(i, _);
    out[i] = mean(noNA(row_vec));
  }
  
  return (out - mean(out)) / sd(out);
}

// [[Rcpp::export]]
List ICC_c(NumericMatrix x) {
  return List::create(Named("ICC", icc_c(x)),
                      Named("row_means", scale_rowmeans(x)));
}
