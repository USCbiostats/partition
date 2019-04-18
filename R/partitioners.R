#' Create a partitioner
#'
#' @description Partitioners are functions that tell the partition algorithm 1)
#'   what to try to reduce 2) how to measure how much information is lost from
#'   the reduction and 3) how to reduce the data. In partition, functions that
#'   handle 1) are called directors, functions that handle 2) are called
#'   metrics,  and functions that handle 3) are called reducers. partition has a
#'   number of pre-specificed partitioners for agglomerative data reduction.
#'   Custom partitioners can be created with [as_partitioner()].
#'
#' @param director a function that directs, possibly created by [as_director()]
#' @param metric a function that measures, possibly created by [as_metric()]
#' @param reducer a function that reduces, possibly created by [as_reducer()]
#'
#' @return a `partitioner`
#' @export
#'
#' @examples
#'
#' as_partitioner(
#'   director = direct_distance_pearson(),
#'   metric = metric_icc(),
#'   reducer = reduce_scaled_means()
#' )
#'
as_partitioner <- function(director, metric, reducer) {
  structure(
    list(
      director = director,
      metric = metric,
      reducer = reducer
    ),
    class = "partitioner"
  )
}

#' Replace the director, metric, or reducer for a partitioner
#'
#' @param partitioner a `partitioner`
#' @inheritParams as_partitioner
#'
#' @return a `partitioner`
#' @export
#'
#' @examples
#'
#' replace_partitioner(
#'   part_icc,
#'   reducer = as_reducer(rowMeans)
#' )
#'
replace_partitioner <- function(partitioner, director = NULL, metric = NULL, reducer = NULL){
  if (!is.null(director)) partitioner$director <- director
  if (!is.null(metric)) partitioner$metric <- metric
  if (!is.null(reducer)) partitioner$reducer <- reducer

  partitioner
}

#' Partitioner: distance, ICC, scaled means
#'
#' @templateVar func `part_icc()`
#' @templateVar director `direct_distance()`, Miniumum Distance
#' @templateVar metric `metric_icc()`, Intraclass Correlation
#' @templateVar reducer `reduce_scaled_mean()`, Scaled Row Means
#' @template describe_partitioner
#'
#' @return a `partitioner`
#' @export
#'
#' @examples
part_icc <- function() {
  as_partitioner(
    director = direct_distance_pearson,
    metric = metric_icc,
    reducer = reduce_scaled_mean
  )
}

#' Partitioner: distance, mutual information, scaled means
#'
#' @templateVar func `part_stdmi()`
#' @templateVar director `direct_distance()`, Miniumum Distance
#' @templateVar metric `metric_std_mutualinfo()`, Standardized Mutual Information
#' @templateVar reducer `reduce_scaled_mean()`, Scaled Row Means
#' @template describe_partitioner
#'
#' @return a `partitioner`
#' @export
#'
#' @examples
part_stdmi <- function() {
  as_partitioner(
    director = direct_distance_pearson,
    metric = metric_std_mutualinfo,
    reducer = reduce_scaled_mean
  )
}

#' Partitioner: distance, minimum R-squared, scaled means
#'
#' @templateVar func `part_minr2()`
#' @templateVar director `direct_distance()`, Miniumum Distance
#' @templateVar metric `metric_min_r2()`, Minimum R-Squared
#' @templateVar reducer `reduce_scaled_mean()`, Scaled Row Means
#' @template describe_partitioner
#'
#' @return a `partitioner`
#' @export
#'
#' @examples
part_minr2 <- function() {
  as_partitioner(
    metric = metric_min_r2,
    director = direct_distance_pearson,
    reducer = reduce_scaled_mean
  )
}

#' Partitioner: distance, first principal component, scaled means
#'
#' @templateVar func `part_minr2()`
#' @templateVar director `direct_distance()`, Miniumum Distance
#' @templateVar metric `metric_variance_explained()`, Variance Explained (PCA)
#' @templateVar reducer `reduce_scaled_mean()`, Scaled Row Means
#' @template describe_partitioner
#'
#' @return a `partitioner`
#' @export
#'
#' @examples
part_pc1 <- function() {
  as_partitioner(
    director = direct_distance_pearson,
    metric = metric_variance_explained,
    reducer = reduce_first_component
  )
}

#' Partitioner: K-means, ICC, scaled means
#'
#' @templateVar func `part_minr2()`
#' @templateVar director `direct_distance()`, Miniumum Distance
#' @templateVar metric `metric_min_icc()`, Minimum Intraclass Correlation
#' @templateVar director `direct_k_cluster()`, K-Means Clusters
#' @templateVar reducer `reduce_kmeans()`, Scaled Row Means
#' @template describe_partitioner
#'
#' @return a `partitioner`
#' @export
#'
#' @examples
part_kmeans <- function() {
  as_partitioner(
    metric = metric_min_icc,
    director = direct_k_cluster,
    reducer = reduce_kmeans
  )
}
