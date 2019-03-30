#ifndef MODSTRING_H
#define MODSTRING_H

#include <Rcpp.h>
Rcpp::NumericVector icc_c(Rcpp::NumericMatrix x);
Rcpp::NumericVector scale_rowmeans(Rcpp::NumericMatrix x);
Rcpp::List ICC_c(Rcpp::NumericMatrix x);

#endif