part_mean_icc <- function() {
  as_partitioner(
    reducer = reduce_scaled_mean,
    metric = metric_icc,
    director = direct_distance_pearson
  )
}

part_mean_mi <- function() {

}

part_mean_minr2 <- function() {

}

part_mean_pc1 <- function() {

}

part_k_means_icc <- function() {
  as_partitioner(
    reducer = reduce_kmeans,
    metric = metric_min_icc,
    director = direct_k_cluster
  )
}
