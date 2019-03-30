// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
List minR2_c(NumericMatrix x) {
  
  NumericVector row_vec(x.ncol()), row_means(x.nrow());
  NumericMatrix cors(x.ncol() + 1), x_means(x.ncol() + 1);
  double r2;
  
  for(int i = 0; i < x.nrow(); ++i) {
    row_vec = x(i, _);
    row_means[i] = mean(noNA(row_vec));
  }
  
  row_means = (row_means - mean(row_means)) / sd(row_means);
  x_means = cbind(row_means, x);
  cors = wrap(arma::cor(as<arma::mat>(x_means)));
  
	for(int i = 0; i < cors.nrow(); ++i) cors(i, _) = pow(cors(i, _), 2);
  
  r2 = min(cors(_, 1));
  
  return List::create(r2, row_means);
}
