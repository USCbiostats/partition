#ifndef PCA_H
#define PCA_H

#include <RcppArmadillo.h>
using namespace Rcpp;
List pca_c(arma::mat x);

#endif