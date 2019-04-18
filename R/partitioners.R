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
#' @param direct a function that directs, possibly created by [as_director()]
#' @param measure a function that measures, possibly created by [as_metric()]
#' @param reduce a function that reduces, possibly created by [as_reducer()]
#'
#' @return a `partitioner`
#' @export
#'
#' @examples
#'
#' as_partitioner(
#'   direct = direct_distance_pearson(),
#'   measure = metric_icc(),
#'   reduce = reduce_scaled_means()
#' )
#'
as_partitioner <- function(direct, measure, reduce) {
  structure(
    list(
      direct = direct,
      measure = measure,
      reduce = reduce
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
#'   reduce = as_reducer(rowMeans)
#' )
#'
replace_partitioner <- function(partitioner, direct = NULL, measure = NULL, reduce = NULL){
  if (!is.null(director)) partitioner$direct <- direct
  if (!is.null(metric)) partitioner$meausure <- measure
  if (!is.null(reducer)) partitioner$reduce <- reduce

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
    direct = direct_distance_pearson,
    measure = metric_icc,
    reduce = reduce_scaled_mean
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
    direct = direct_distance_pearson,
    measure = metric_std_mutualinfo,
    reduce = reduce_scaled_mean
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
    direct = direct_distance_pearson,
    measure = metric_min_r2,
    reduce = reduce_scaled_mean
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
    direct = direct_distance_pearson,
    measure = metric_variance_explained,
    reduce = reduce_first_component
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
    direct = direct_k_cluster,
    measure = metric_min_icc,
    reduce = reduce_kmeans
  )
}
