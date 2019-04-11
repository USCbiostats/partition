// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <RcppMLPACK.h>
#include <Rcpp.h>

using namespace Rcpp;

// arma_kmeans
List arma_kmeans(arma::mat x, int k);
RcppExport SEXP _partition2_arma_kmeans(SEXP xSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(arma_kmeans(x, k));
    return rcpp_result_gen;
END_RCPP
}
// rank_c
Rcpp::NumericVector rank_c(Rcpp::NumericVector x);
RcppExport SEXP _partition2_rank_c(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rank_c(x));
    return rcpp_result_gen;
END_RCPP
}
// apply_rank
arma::mat apply_rank(arma::mat x);
RcppExport SEXP _partition2_apply_rank(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(apply_rank(x));
    return rcpp_result_gen;
END_RCPP
}
// corr_c_mat
arma::mat corr_c_mat(arma::mat x);
RcppExport SEXP _partition2_corr_c_mat(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(corr_c_mat(x));
    return rcpp_result_gen;
END_RCPP
}
// corr_c_2mat
arma::mat corr_c_2mat(arma::mat x, arma::mat y);
RcppExport SEXP _partition2_corr_c_2mat(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(corr_c_2mat(x, y));
    return rcpp_result_gen;
END_RCPP
}
// corr_c_2vec
double corr_c_2vec(arma::vec x, arma::vec y);
RcppExport SEXP _partition2_corr_c_2vec(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(corr_c_2vec(x, y));
    return rcpp_result_gen;
END_RCPP
}
// pearson_distance
NumericMatrix pearson_distance(NumericMatrix x, NumericMatrix y);
RcppExport SEXP _partition2_pearson_distance(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(pearson_distance(x, y));
    return rcpp_result_gen;
END_RCPP
}
// spearman_distance
NumericMatrix spearman_distance(NumericMatrix x, NumericMatrix y);
RcppExport SEXP _partition2_spearman_distance(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(spearman_distance(x, y));
    return rcpp_result_gen;
END_RCPP
}
// icc_c
double icc_c(NumericMatrix x);
RcppExport SEXP _partition2_icc_c(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(icc_c(x));
    return rcpp_result_gen;
END_RCPP
}
// scale_rowmeans
NumericVector scale_rowmeans(NumericMatrix x);
RcppExport SEXP _partition2_scale_rowmeans(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(scale_rowmeans(x));
    return rcpp_result_gen;
END_RCPP
}
// ICC_c
List ICC_c(NumericMatrix x);
RcppExport SEXP _partition2_ICC_c(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(ICC_c(x));
    return rcpp_result_gen;
END_RCPP
}
// subset_matrix
NumericMatrix subset_matrix(List& x, int& i, arma::mat& m);
RcppExport SEXP _partition2_subset_matrix(SEXP xSEXP, SEXP iSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List& >::type x(xSEXP);
    Rcpp::traits::input_parameter< int& >::type i(iSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(subset_matrix(x, i, m));
    return rcpp_result_gen;
END_RCPP
}
// min_icc_c
NumericVector min_icc_c(List& columns, arma::mat& x, int& k, double& threshold);
RcppExport SEXP _partition2_min_icc_c(SEXP columnsSEXP, SEXP xSEXP, SEXP kSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List& >::type columns(columnsSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< int& >::type k(kSEXP);
    Rcpp::traits::input_parameter< double& >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(min_icc_c(columns, x, k, threshold));
    return rcpp_result_gen;
END_RCPP
}
// kmeans_c
arma::mat kmeans_c(arma::mat& x, int k, int n_iter, bool verbose, int seed);
RcppExport SEXP _partition2_kmeans_c(SEXP xSEXP, SEXP kSEXP, SEXP n_iterSEXP, SEXP verboseSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type n_iter(n_iterSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(kmeans_c(x, k, n_iter, verbose, seed));
    return rcpp_result_gen;
END_RCPP
}
// assign_cluster
NumericVector assign_cluster(arma::mat& x, arma::mat init_centroids);
RcppExport SEXP _partition2_assign_cluster(SEXP xSEXP, SEXP init_centroidsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type init_centroids(init_centroidsSEXP);
    rcpp_result_gen = Rcpp::wrap(assign_cluster(x, init_centroids));
    return rcpp_result_gen;
END_RCPP
}
// kmean_assignment
NumericVector kmean_assignment(arma::mat& x, int k, int n_iter, bool verbose, int seed);
RcppExport SEXP _partition2_kmean_assignment(SEXP xSEXP, SEXP kSEXP, SEXP n_iterSEXP, SEXP verboseSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type n_iter(n_iterSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(kmean_assignment(x, k, n_iter, verbose, seed));
    return rcpp_result_gen;
END_RCPP
}
// minR2_c
List minR2_c(NumericMatrix x);
RcppExport SEXP _partition2_minR2_c(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(minR2_c(x));
    return rcpp_result_gen;
END_RCPP
}
// update_dist
NumericMatrix update_dist(NumericMatrix reduced_dist, CharacterVector cluster_nm, CharacterVector clust_var_nms, NumericMatrix reduced_data, std::string dist_type);
RcppExport SEXP _partition2_update_dist(SEXP reduced_distSEXP, SEXP cluster_nmSEXP, SEXP clust_var_nmsSEXP, SEXP reduced_dataSEXP, SEXP dist_typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type reduced_dist(reduced_distSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type cluster_nm(cluster_nmSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type clust_var_nms(clust_var_nmsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type reduced_data(reduced_dataSEXP);
    Rcpp::traits::input_parameter< std::string >::type dist_type(dist_typeSEXP);
    rcpp_result_gen = Rcpp::wrap(update_dist(reduced_dist, cluster_nm, clust_var_nms, reduced_data, dist_type));
    return rcpp_result_gen;
END_RCPP
}
// assign_clusters
List assign_clusters(IntegerVector index_r, NumericMatrix reduced_dist_r, NumericMatrix reduced_data_r, NumericMatrix data_r, double pct_var, DataFrame clusters_r, int cluster_ind, std::string method, std::string dist_type, std::string new_var);
RcppExport SEXP _partition2_assign_clusters(SEXP index_rSEXP, SEXP reduced_dist_rSEXP, SEXP reduced_data_rSEXP, SEXP data_rSEXP, SEXP pct_varSEXP, SEXP clusters_rSEXP, SEXP cluster_indSEXP, SEXP methodSEXP, SEXP dist_typeSEXP, SEXP new_varSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type index_r(index_rSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type reduced_dist_r(reduced_dist_rSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type reduced_data_r(reduced_data_rSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data_r(data_rSEXP);
    Rcpp::traits::input_parameter< double >::type pct_var(pct_varSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type clusters_r(clusters_rSEXP);
    Rcpp::traits::input_parameter< int >::type cluster_ind(cluster_indSEXP);
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< std::string >::type dist_type(dist_typeSEXP);
    Rcpp::traits::input_parameter< std::string >::type new_var(new_varSEXP);
    rcpp_result_gen = Rcpp::wrap(assign_clusters(index_r, reduced_dist_r, reduced_data_r, data_r, pct_var, clusters_r, cluster_ind, method, dist_type, new_var));
    return rcpp_result_gen;
END_RCPP
}
// pca_c
List pca_c(arma::mat x);
RcppExport SEXP _partition2_pca_c(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(pca_c(x));
    return rcpp_result_gen;
END_RCPP
}
// reduce_partition_c
List reduce_partition_c(List x, DataFrame df, Function assign_partition, List partitioner, const double& threshold, const double& tolerance, const std::string& var_prefix, const int& niter);
RcppExport SEXP _partition2_reduce_partition_c(SEXP xSEXP, SEXP dfSEXP, SEXP assign_partitionSEXP, SEXP partitionerSEXP, SEXP thresholdSEXP, SEXP toleranceSEXP, SEXP var_prefixSEXP, SEXP niterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< Function >::type assign_partition(assign_partitionSEXP);
    Rcpp::traits::input_parameter< List >::type partitioner(partitionerSEXP);
    Rcpp::traits::input_parameter< const double& >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< const double& >::type tolerance(toleranceSEXP);
    Rcpp::traits::input_parameter< const std::string& >::type var_prefix(var_prefixSEXP);
    Rcpp::traits::input_parameter< const int& >::type niter(niterSEXP);
    rcpp_result_gen = Rcpp::wrap(reduce_partition_c(x, df, assign_partition, partitioner, threshold, tolerance, var_prefix, niter));
    return rcpp_result_gen;
END_RCPP
}
// bind_empty_row
NumericMatrix bind_empty_row(NumericMatrix x);
RcppExport SEXP _partition2_bind_empty_row(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(bind_empty_row(x));
    return rcpp_result_gen;
END_RCPP
}
// has_rownames
bool has_rownames(NumericMatrix x);
RcppExport SEXP _partition2_has_rownames(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(has_rownames(x));
    return rcpp_result_gen;
END_RCPP
}
// has_colnames
bool has_colnames(NumericMatrix x);
RcppExport SEXP _partition2_has_colnames(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(has_colnames(x));
    return rcpp_result_gen;
END_RCPP
}
// subset_c
NumericMatrix subset_c(NumericMatrix x, IntegerVector subset);
RcppExport SEXP _partition2_subset_c(SEXP xSEXP, SEXP subsetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type subset(subsetSEXP);
    rcpp_result_gen = Rcpp::wrap(subset_c(x, subset));
    return rcpp_result_gen;
END_RCPP
}
// drop
NumericMatrix drop(NumericMatrix x, IntegerVector subset);
RcppExport SEXP _partition2_drop(SEXP xSEXP, SEXP subsetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type subset(subsetSEXP);
    rcpp_result_gen = Rcpp::wrap(drop(x, subset));
    return rcpp_result_gen;
END_RCPP
}
// drop_dist
NumericMatrix drop_dist(NumericMatrix reduced_dist, IntegerVector distance_index);
RcppExport SEXP _partition2_drop_dist(SEXP reduced_distSEXP, SEXP distance_indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type reduced_dist(reduced_distSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type distance_index(distance_indexSEXP);
    rcpp_result_gen = Rcpp::wrap(drop_dist(reduced_dist, distance_index));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_partition2_arma_kmeans", (DL_FUNC) &_partition2_arma_kmeans, 2},
    {"_partition2_rank_c", (DL_FUNC) &_partition2_rank_c, 1},
    {"_partition2_apply_rank", (DL_FUNC) &_partition2_apply_rank, 1},
    {"_partition2_corr_c_mat", (DL_FUNC) &_partition2_corr_c_mat, 1},
    {"_partition2_corr_c_2mat", (DL_FUNC) &_partition2_corr_c_2mat, 2},
    {"_partition2_corr_c_2vec", (DL_FUNC) &_partition2_corr_c_2vec, 2},
    {"_partition2_pearson_distance", (DL_FUNC) &_partition2_pearson_distance, 2},
    {"_partition2_spearman_distance", (DL_FUNC) &_partition2_spearman_distance, 2},
    {"_partition2_icc_c", (DL_FUNC) &_partition2_icc_c, 1},
    {"_partition2_scale_rowmeans", (DL_FUNC) &_partition2_scale_rowmeans, 1},
    {"_partition2_ICC_c", (DL_FUNC) &_partition2_ICC_c, 1},
    {"_partition2_subset_matrix", (DL_FUNC) &_partition2_subset_matrix, 3},
    {"_partition2_min_icc_c", (DL_FUNC) &_partition2_min_icc_c, 4},
    {"_partition2_kmeans_c", (DL_FUNC) &_partition2_kmeans_c, 5},
    {"_partition2_assign_cluster", (DL_FUNC) &_partition2_assign_cluster, 2},
    {"_partition2_kmean_assignment", (DL_FUNC) &_partition2_kmean_assignment, 5},
    {"_partition2_minR2_c", (DL_FUNC) &_partition2_minR2_c, 1},
    {"_partition2_update_dist", (DL_FUNC) &_partition2_update_dist, 5},
    {"_partition2_assign_clusters", (DL_FUNC) &_partition2_assign_clusters, 10},
    {"_partition2_pca_c", (DL_FUNC) &_partition2_pca_c, 1},
    {"_partition2_reduce_partition_c", (DL_FUNC) &_partition2_reduce_partition_c, 8},
    {"_partition2_bind_empty_row", (DL_FUNC) &_partition2_bind_empty_row, 1},
    {"_partition2_has_rownames", (DL_FUNC) &_partition2_has_rownames, 1},
    {"_partition2_has_colnames", (DL_FUNC) &_partition2_has_colnames, 1},
    {"_partition2_subset_c", (DL_FUNC) &_partition2_subset_c, 2},
    {"_partition2_drop", (DL_FUNC) &_partition2_drop, 2},
    {"_partition2_drop_dist", (DL_FUNC) &_partition2_drop_dist, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_partition2(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
