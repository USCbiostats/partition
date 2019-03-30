#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

LogicalVector is_element(CharacterVector x, CharacterVector elements) {
  LogicalVector any_match(x.size()), element_match(elements.size());

  for(int i = 0; i < x.size(); ++i) {
   for(int k = 0; k < elements.size(); ++k) {
     element_match[k] = x[i] == elements[k];
   }
   any_match[i] = is_true(any(element_match));
  }
  return any_match;
}

IntegerVector which(LogicalVector x) {
  IntegerVector out;
  
  for(int i = 0; i < x.size(); i++) {
    if(x[i] == TRUE) {
      out.push_back(i);
     }
    }
  return out;
}

// [[Rcpp::export]]
NumericMatrix bind_empty_row(NumericMatrix x) {
  NumericVector row_vec(x.ncol());
  row_vec = rep(NumericVector::get_na(), x.ncol());
  NumericMatrix out(x.rows() + 1, x.cols());
  out = transpose(cbind(transpose(x), row_vec));
  return out;
}

// [[Rcpp::export]]
bool has_rownames(NumericMatrix x) {
  return !Rf_isNull(rownames(x));
}

// [[Rcpp::export]]
bool has_colnames(NumericMatrix x) {
  return !Rf_isNull(colnames(x));
}

// [[Rcpp::export]]
NumericMatrix subset_c(NumericMatrix x, IntegerVector subset) {
  NumericMatrix out(x.nrow(), subset.size());

  for (int i = 0; i < subset.size(); i++) {
    out.column(i) = x(_, subset[i]);
  }
  
  if (has_colnames(x)) {
    CharacterVector column_names, reduced_names;
    column_names = colnames(x);
    colnames(out) = column_names[subset];
  }
  
  if (has_rownames(x)) {
    CharacterVector row_names;
    row_names = rownames(x);
    rownames(out) = row_names;
  }
  
  return out;
}

// [[Rcpp::export]]
NumericMatrix drop(NumericMatrix x, IntegerVector subset) {

  NumericMatrix out(x.nrow(), x.ncol() - subset.size());
  LogicalVector should_drop(subset.size());
  
  int j = 0;
  for (int i = 0; i < x.ncol(); i++) {
    for (int k = 0; k < subset.size(); k++) {
      should_drop[k] = subset[k] == i;
    }
    
    if (is_false(any(should_drop))) {
      out.column(j) = x(_, i);
      j++;
    }
  }
  
  if (has_colnames(x)) {
    CharacterVector column_names;
    column_names = colnames(x);
    
    for (int k = 0; k < subset.size(); k++) {
      column_names.erase(subset[k]);
      subset = subset - 1;
    }
    
    colnames(out) = column_names;
  }
  
  if (has_rownames(x)) {
    CharacterVector row_names;
    row_names = rownames(x);
    rownames(out) = row_names;
  }
  
  return out;
}

// [[Rcpp::export]]
NumericMatrix drop_dist(NumericMatrix reduced_dist, IntegerVector distance_index) {
 IntegerVector distance_index_rows;
 distance_index_rows = clone(distance_index);
 reduced_dist = drop(reduced_dist, distance_index);
 reduced_dist = transpose(drop(transpose(reduced_dist), distance_index_rows));
 return reduced_dist;
}