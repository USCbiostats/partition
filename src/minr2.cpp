// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
List minR2_c(NumericMatrix x) {

  NumericVector row_means(x.nrow()), std_row_means(x.nrow());
  NumericMatrix cors(x.ncol() + 1), x_means(x.ncol() + 1);
  double r2;

  row_means = rowMeans(x, true);

  std_row_means = (na_omit(row_means) - mean(na_omit(row_means))) / sd(na_omit(row_means));
  x_means = cbind(std_row_means, x);
  cors = wrap(arma::cor(as<arma::mat>(x_means)));

	NumericVector cors_vec = cors(_, 0);

  r2 = min(pow(cors_vec, 2));

  return List::create(_["minr2"] = r2, _["row_means"] = std_row_means);
}
