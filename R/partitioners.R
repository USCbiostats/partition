part_mean_icc <- function() {
  as_partitioner(
    reducer = reduce_scaled_mean,
    metric = metric_icc,
    director = direct_distance_pearson
  )
}

part_mean_stdmi <- function() {
  as_partitioner(
    reducer = reduce_scaled_mean,
    metric = metric_std_mutualinfo,
    director = direct_distance_pearson
  )
}

part_mean_minr2 <- function() {
  as_partitioner(
    reducer = reduce_scaled_mean,
    metric = metric_min_r2,
    director = direct_distance_pearson
  )
}

part_mean_pc1 <- function() {
  as_partitioner(
    reducer = reduce_first_component,
    metric = metric_variance_explained,
    director = direct_distance_pearson
  )
}

part_k_means_icc <- function() {
  as_partitioner(
    reducer = reduce_kmeans,
    metric = metric_min_icc,
    director = direct_k_cluster
  )
}
