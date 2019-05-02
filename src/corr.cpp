// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

class SortRanks {
private:
    const Rcpp::NumericVector& ref;

    bool is_na(double x) const
    {
        return Rcpp::traits::is_na<REALSXP>(x);
    }

public:
    SortRanks(const Rcpp::NumericVector& ref_)
        : ref(ref_)
    {}

    bool operator()(const int ilhs, const int irhs) const
    {
        double lhs = ref[ilhs], rhs = ref[irhs];
        if (is_na(lhs)) return false;
        if (is_na(rhs)) return true;
        return lhs < rhs;
    }
};

// [[Rcpp::export]]
Rcpp::NumericVector rank_c(Rcpp::NumericVector x)
{
    int vec_size = x.size();
    Rcpp::IntegerVector ranks = seq(0, vec_size - 1);
    std::sort(ranks.begin(), ranks.end(), SortRanks(x));

    Rcpp::NumericVector avg_ranks(vec_size);
    for (int n, i = 0; i < vec_size; i += n) {
        n = 1;
        while (i + n < vec_size && x[ranks[i]] == x[ranks[i + n]]) ++n;
        for (int k = 0; k < n; k++) {
            avg_ranks[ranks[i + k]] = i + (n + 1) / 2.0;
        }
    }

    return avg_ranks;
}

// [[Rcpp::export]]
arma::mat apply_rank(arma::mat x) {
  NumericVector temp_vec;
  NumericMatrix out(x.n_rows, x.n_cols);
  NumericMatrix y = wrap(x);

  for(int i = 0; i < y.ncol(); ++i) {
    temp_vec = y(_, i);
    out(_, i) = rank_c(temp_vec);
  }

  return as<arma::mat>(out);
}

// [[Rcpp::export]]
arma::mat corr_c_mat(arma::mat x) {
  return arma::cor(x);
}

// [[Rcpp::export]]
arma::mat corr_c_2mat(arma::mat x, arma::mat y) {
  return arma::cor(x, y);
}

// [[Rcpp::export]]
double corr_c_2vec(arma::vec x, arma::vec y) {
  if (size(x) != size(y)) {
    throw Rcpp::exception("cor(): x and y are not the same length!", false);
  }

  arma::mat out;
  out = arma::cor(x, y);
  return out(0);
}
