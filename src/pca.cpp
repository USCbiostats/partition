// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
List pca_c(arma::mat x) {
  NumericMatrix rcpp_x = wrap(x);
  NumericVector col_vec;

  // scale each variable
  for(int i = 0; i < rcpp_x.ncol(); ++i) {
    col_vec = rcpp_x(_, i);
    rcpp_x(_, i) = (col_vec - mean(na_omit(col_vec))) / sd(na_omit(col_vec));
  }

 x = as<arma::mat>(rcpp_x);

 mat coeff;
 mat score;
 vec latent, pc1;
 double pct_var;

 arma::princomp(coeff, score, latent, x);

 pc1 = score.col(0);
 pc1 = (pc1 - mean(pc1)) / stddev(pc1);
 pct_var = latent[0] / sum(latent);

 return List::create(_["pct_var"] = pct_var, _["pc1"] = pc1);
}
