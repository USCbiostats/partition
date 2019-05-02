#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List reduce_partition_c(List x, DataFrame df, Function assign_partition,
                        List partitioner, const double &threshold, const double &tolerance, const std::string &var_prefix, const int &niter) {
  int streak = 0;
  double metric;
  bool all_done;

  while (streak <= niter) {
    x = assign_partition(x, partitioner, df, threshold, tolerance, var_prefix);

    all_done = x["all_done"];
    if (all_done) break;

    metric = x["metric"];

    if (metric < threshold) streak++; else streak = 0;
  }

  return x;
}

