#ifndef UTILS_H
#define UTILS_H

#include <RcppArmadillo.h>
using namespace Rcpp;
LogicalVector is_element(CharacterVector x, CharacterVector elements);
IntegerVector which(LogicalVector x);
NumericMatrix bind_empty_row(NumericMatrix x);
bool has_rownames(NumericMatrix x);
bool has_colnames(NumericMatrix x);
NumericMatrix subset_c(NumericMatrix x, IntegerVector subset);
NumericMatrix drop(NumericMatrix x, IntegerVector subset);
NumericMatrix drop_dist(NumericMatrix reduced_dist, IntegerVector distance_index);

#endif