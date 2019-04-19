#' Create a custom metric
#'
#' @template describe_metric
#'
#' @param .f a function that returns either a numeric vector or a `data.frame`
#' @param ... Extra arguments passed to `.f`.
#'
#' @return a function to use in [`as_partitioner()`]
#' @export
#'
#' @examples
#'
#' inter_item_reliability <- function(.data) {
#'    corr(.data) %>%
#'     colMeans(na.rm = TRUE) %>%
#'     mean()
#' }
#'
#' metric_iir <- as_metric(inter_item_reliability)
#' metric_iir
#'
#' @family metrics
as_metric <- function(.f, ...) {
  function(.partition_step, ...) {
    if (.partition_step$all_done) return(.partition_step)

    composite_variables <- pull_composite_variables(.partition_step)
    target_data <- .partition_step$.df[, composite_variables]

    .partition_step$metric <- .f(target_data, ...)

    .partition_step
  }
}

#' Measure the information loss of reduction using intraclass correlation
#' coefficient
#'
#' @template describe_metric
#'
#' @description `metric_icc()` assesses information loss by calculating the
#'   intraclass correlation coefficient for the target variables.
#'
#' @template partition_step
#' @export
metric_icc <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  .partition_step$metric <- icc_r(as.matrix(target_data))

  .partition_step
}

#' Measure the information loss of reduction using the minimum intraclass
#' correlation coefficient
#'
#' @template describe_metric
#'
#' @description `metric_min_icc()` assesses information loss by calculating the
#'   intraclass correlation coefficient for each set of the target variables and
#'   finding their minimum.
#'
#' @template partition_step
#' @param search_method The search method. Binary search is generally more efficient
#'   but linear search can be faster in very low dimensions.
#'
#' @export
metric_min_icc <- function(.partition_step, search_method = c("binary", "linear")) {
  search_method <- match.arg(search_method)
  if (.partition_step$all_done) return(.partition_step)

  # get indices for each cluster as list and subtract by one for cpp indexing
  indices <- purrr::map(
    seq_len(.partition_step$k),
    ~which(.partition_step$target == .x) - 1
  )

  # calculate ICC for each cluster
  # (note: stops early when ICC under threshold, returning 0s for the rest)
  k_icc <- min_icc_c(
    indices,
    as.matrix(.partition_step$.df),
    .partition_step$k,
    .partition_step$threshold
  )

  # use minimum icc as metric
  .partition_step$metric <- min(k_icc)
  # store vector of icc for mappings if min icc > threshold
  .partition_step$metric_vector <- k_icc

  if (search_method == "binary") {
    # for binary k search, also find the metrics for k - 1
    # get indices for each cluster as list and subtract by one for cpp indexing
    indices_k1 <- purrr::map(
      seq_len(.partition_step$k - 1),
      ~which(.partition_step$target_k1 == .x) - 1
    )

    # calculate ICC for each cluster
    # (note: stops early when ICC under threshold, returning 0s for the rest)
    k_icc_k1 <- min_icc_c(
      indices_k1,
      as.matrix(.partition_step$.df),
      .partition_step$k - 1,
      .partition_step$threshold
    )

    # use minimum icc as metric
    .partition_step$metric_k1 <- min(k_icc_k1)
    # store vector of icc for mappings if min icc > threshold
    .partition_step$metric_vector_k1 <- k_icc_k1
  }

  .partition_step
}

#' Measure the information loss of reduction using the variance explained
#'
#' @template describe_metric
#'
#' @description `metric_variance_explained()` assesses information loss by calculating the
#'   variance explained by the first component of a principal components analysis.
#'
#' @template partition_step
#' @export
metric_variance_explained <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  pca1 <- pca_c(as.matrix(target_data))
  .partition_step$metric <- pca1[["pct_var"]]
  # PCA and variance explained are calculated at the same time for efficiency
  # so store the first PC to use later as the reduced variable
  uses_first_component <- is_same_function(.partition_step$partitioner$reduce, reduce_first_component)
  if (uses_first_component) .partition_step$new_variable <- as.numeric(pca1[["pc1"]])

  .partition_step
}

#' Measure the information loss of reduction using minimum R-squared
#'
#' @template describe_metric
#'
#' @description `metric_variance_explained()` assesses information loss by
#'   calculating the minimum R-squared for the target variables.
#'
#' @template partition_step
#' @export
metric_min_r2 <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  minr2 <- minR2_c(as.matrix(target_data))
  .partition_step$metric <- minr2[["minr2"]]
  # we need scaled means for min r2 calculation; store it to use for reducing
  uses_scaled_mean <- is_same_function(.partition_step$partitioner$reduce, reduce_scaled_mean)
  if (uses_scaled_mean) .partition_step$new_variable <- minr2[["row_means"]]

  .partition_step
}

#' Measure the information loss of reduction using standardized mutual
#' information
#'
#' @template describe_metric
#'
#' @description `metric_std_mutualinfo()` assesses information loss by
#'   calculating the standardized mutual information for the target variables.
#'   See [mutual_information()].
#'
#' @template partition_step
#' @export
metric_std_mutualinfo <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  mi <- mutual_information(target_data)
  .partition_step$metric <- mi[["standardized_mi"]]
  # we need scaled means for MI calculation; store it to use for reducing
  uses_scaled_mean <- is_same_function(.partition_step$partitioner$reduce, reduce_scaled_mean)
  if (uses_scaled_mean) .partition_step$new_variable <- mi[["scaled_row_means"]]

  .partition_step
}


#' Calculate the intraclass correlation coefficient
#'
#' `icc()` efficiently calculates the ICC for a numeric data set.
#'
#' @param .x a data set
#' @param method The method source: both the pure R and C++ versions are efficient
#'
#' @return a numeric vector of length 1
#' @export
#'
#' @examples
#' library(dplyr)
#' iris %>%
#'   select_if(is.numeric) %>%
#'   icc()
icc <- function(.x, method = c("r", "c")) {
  method <- match.arg(method)
  .x <- as.matrix(.x)

  if (method == "c") return(icc_c(.x))

  icc_r(.x)
}

#' Calculate the intraclass correlation coefficient
#'
#' `icc_r()` efficiently calculates the ICC for a numeric data set in pure R.
#'
#' @param .x a data set
#' @return a numeric vector of length 1
#' @keywords internal
icc_r <- function(.x) {
  ncols <- ncol(.x)
  nrows <- nrow(.x)

  rowmeans <- rowMeans(.x)
  long_means <- rep(rowmeans, ncols)

  within <- (as.numeric(.x) - long_means)^2
  among <- (long_means - mean(.x))^2

  ms1 <- sum(among) / (nrows - 1)
  ms2 <- sum(within) / (nrows * (ncols - 1))

  variance <- (ms1 - ms2) / ncols

  variance / (variance + ms2)
}
