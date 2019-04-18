#ifndef CORR_H
#define CORR_H

#include <RcppArmadillo.h>
Rcpp::NumericVector rank_c(Rcpp::NumericVector x);
arma::mat apply_rank(arma::mat x);
arma::mat corr_c_mat(arma::mat x);
arma::mat corr_c_2mat(arma::mat x, arma::mat y);
double corr_c_2vec(arma::vec x, arma::vec y);
NumericMatrix pearson_distance(NumericMatrix x, NumericMatrix y);
NumericMatrix spearman_distance(NumericMatrix x, NumericMatrix y);

#endif