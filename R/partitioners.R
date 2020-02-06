#' Create a partitioner
#'
#' @template describe_as_partitioner
#'
#' @param direct a function that directs, possibly created by [as_director()]
#' @param measure a function that measures, possibly created by [as_measure()]
#' @param reduce a function that reduces, possibly created by [as_reducer()]
#'
#' @return a `partitioner`
#' @export
#'
#' @examples
#'
#' as_partitioner(
#'   direct = direct_distance_pearson,
#'   measure = measure_icc,
#'   reduce = reduce_scaled_mean
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
#' @family partitioners
replace_partitioner <- function(partitioner, direct = NULL, measure = NULL, reduce = NULL) {
  if (!is_partitioner(partitioner) && is.function(partitioner)) partitioner <- partitioner()

  if (!is.null(direct)) partitioner$direct <- direct
  if (!is.null(measure)) partitioner$measure <- measure
  if (!is.null(reduce)) partitioner$reduce <- reduce

  partitioner
}

#' Is this object a partitioner?
#'
#' @param x an object to be tested
#'
#' @return logical: `TRUE` or `FALSE`
#' @export
is_partitioner <- function(x) inherits(x, "partitioner")

#' Partitioner: distance, ICC, scaled means
#'
#' @template describe_as_partitioner
#' @templateVar func part_icc()
#' @templateVar director `direct_distance()`, Minimum Distance
#' @templateVar metric `measure_icc()`, Intraclass Correlation
#' @templateVar reducer `reduce_scaled_mean()`, Scaled Row Means
#' @template describe_partitioner
#' @param spearman logical. Use Spearman's correlation for distance matrix?
#'
#' @return a `partitioner`
#' @export
part_icc <- function(spearman = FALSE) {
  as_partitioner(
    direct = direct_dist(spearman = spearman),
    measure = measure_icc,
    reduce = reduce_scaled_mean
  )
}

#' Partitioner: distance, mutual information, scaled means
#'
#' @template describe_as_partitioner
#' @templateVar func part_stdmi()
#' @templateVar director `direct_distance()`, Minimum Distance
#' @templateVar metric `measure_std_mutualinfo()`, Standardized Mutual Information
#' @templateVar reducer `reduce_scaled_mean()`, Scaled Row Means
#' @template describe_partitioner
#' @inheritParams part_icc
#'
#' @return a `partitioner`
#' @export
part_stdmi <- function(spearman = FALSE) {
  as_partitioner(
    direct = direct_dist(spearman = spearman),
    measure = measure_std_mutualinfo,
    reduce = reduce_scaled_mean
  )
}

#' Partitioner: distance, minimum R-squared, scaled means
#'
#' @template describe_as_partitioner
#' @templateVar func part_minr2()
#' @templateVar director `direct_distance()`, Minimum Distance
#' @templateVar metric `measure_min_r2()`, Minimum R-Squared
#' @templateVar reducer `reduce_scaled_mean()`, Scaled Row Means
#' @template describe_partitioner
#' @inheritParams part_icc
#'
#' @return a `partitioner`
#' @export
part_minr2 <- function(spearman = FALSE) {
  as_partitioner(
    direct = direct_dist(spearman = spearman),
    measure = measure_min_r2,
    reduce = reduce_scaled_mean
  )
}

#' Partitioner: distance, first principal component, scaled means
#'
#' @template describe_as_partitioner
#' @templateVar func part_pc1()
#' @templateVar director `direct_distance()`, Minimum Distance
#' @templateVar metric `measure_variance_explained()`, Variance Explained (PCA)
#' @templateVar reducer `reduce_first_component()`, First Principal Component
#' @template describe_partitioner
#' @inheritParams part_icc
#'
#' @return a `partitioner`
#' @export
part_pc1 <- function(spearman = FALSE) {
  as_partitioner(
    direct = direct_dist(spearman = spearman),
    measure = measure_variance_explained,
    reduce = reduce_first_component
  )
}

#' Partitioner: K-means, ICC, scaled means
#'
#' @template describe_as_partitioner
#' @templateVar func part_kmeans()
#' @templateVar director `direct_distance()`, Minimum Distance
#' @templateVar metric `measure_min_icc()`, Minimum Intraclass Correlation
#' @templateVar director `direct_k_cluster()`, K-Means Clusters
#' @templateVar reducer `reduce_kmeans()`, Scaled Row Means
#' @template describe_partitioner
#' @inheritParams direct_k_cluster
#' @inheritParams reduce_kmeans
#'
#' @return a `partitioner`
#' @export
part_kmeans <- function(algorithm = c("armadillo", "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
                        search = c("binary", "linear"),
                        init_k = NULL,
                        n_hits = 4) {
  algorithm <- match.arg(algorithm)
  search <- match.arg(search)

  as_partitioner(
    direct = purrr::partial(
      direct_k_cluster,
      algorithm = algorithm,
      search = search,
      init_k = init_k
    ),
    measure = purrr::partial(
      measure_min_icc,
      search_method = search
    ),
    reduce = purrr::partial(
      reduce_kmeans,
      search = search,
      n_hits = n_hits
    )
  )
}

direct_dist <- function(spearman) {
  #  return the correct director
  if (spearman) {
    direct_dist_f <- direct_distance_spearman
  } else {
    direct_dist_f <- direct_distance_pearson
  }

  direct_dist_f
}
