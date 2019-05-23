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
#' measure_iir <- as_measure(inter_item_reliability)
#' measure_iir
#'
#' @family metrics
as_measure <- function(.f, ...) {
  function(.partition_step, ...) {
    if (.partition_step$all_done) return(.partition_step)

    composite_variables <- pull_composite_variables(.partition_step)
    target_data <- .partition_step$.df[, composite_variables, drop = FALSE]

    .partition_step$metric <- .f(target_data, ...)

    .partition_step
  }
}

#' Measure the information loss of reduction using intraclass correlation
#' coefficient
#'
#' @template describe_metric
#'
#' @description `measure_icc()` assesses information loss by calculating the
#'   intraclass correlation coefficient for the target variables.
#'
#' @template partition_step
#' @export
measure_icc <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables, drop = FALSE]

  .partition_step$metric <- icc_r(as.matrix(target_data))

  .partition_step
}

#' Measure the information loss of reduction using the minimum intraclass
#' correlation coefficient
#'
#' @template describe_metric
#'
#' @description `measure_min_icc()` assesses information loss by calculating the
#'   intraclass correlation coefficient for each set of the target variables and
#'   finding their minimum.
#'
#' @template partition_step
#' @param search_method The search method. Binary search is generally more efficient
#'   but linear search can be faster in very low dimensions.
#'
#' @export
measure_min_icc <- function(.partition_step, search_method = c("binary", "linear")) {
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
  # check number of hits below threshold
  if (under_threshold(.partition_step)) .partition_step <- increase_hits(.partition_step)
  #  store iteration if above threshold
  if (above_threshold(.partition_step)) {
    .partition_step$last_target <- list(
      target = .partition_step$target,
      k = .partition_step$k,
      metric = .partition_step$metric_vector
    )
  }

  if (.partition_step$k == 1 && under_threshold(.partition_step)) {
    return(all_done(.partition_step))
  }

  if (.partition_step$k == 1 && search_method == "binary") {
    .partition_step$metric_k1 <- 0
    return(.partition_step)
  }

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
#' @description `measure_variance_explained()` assesses information loss by calculating the
#'   variance explained by the first component of a principal components analysis.
#'
#' @template partition_step
#' @export
measure_variance_explained <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables, drop = FALSE]

  pca1 <- pca_c(as.matrix(na.omit(target_data)))
  .partition_step$metric <- pca1[["pct_var"]]
  # PCA and variance explained are calculated at the same time for efficiency
  # so store the first PC to use later as the reduced variable
  uses_first_component <- is_same_function(.partition_step$partitioner$reduce, reduce_first_component)
  if (uses_first_component) {
    .partition_step$new_variable <- as.numeric(pca1[["pc1"]])
    missing_row <- is_missing_rowwise(target_data)
    #  return same number of rows as original data with missing values where not complete
    if (any(missing_row)) .partition_step$new_variable <- fill_in_missing(.partition_step$new_variable, missing_row)
  }

  .partition_step
}

#' Measure the information loss of reduction using minimum R-squared
#'
#' @template describe_metric
#'
#' @description `measure_min_r2()` assesses information loss by
#'   calculating the minimum R-squared for the target variables.
#'
#' @template partition_step
#' @export
measure_min_r2 <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables, drop = FALSE]

  minr2 <- minR2_c(as.matrix(na.omit(target_data)))
  .partition_step$metric <- minr2[["minr2"]]
  # we need scaled means for min r2 calculation; store it to use for reducing
  uses_scaled_mean <- is_same_function(.partition_step$partitioner$reduce, reduce_scaled_mean)
  if (uses_scaled_mean) {
    .partition_step$new_variable <- minr2[["row_means"]]
    missing_rows <- is_missing_rowwise(target_data)
    if (any(missing_rows)) {
      missing_means <- scaled_mean_r(target_data[missing_rows, ])
      .partition_step$new_variable <- fill_in_missing(
        .partition_step$new_variable,
        missing_rows,
        missing_means
      )
    }
  }

  .partition_step
}

#' Measure the information loss of reduction using standardized mutual
#' information
#'
#' @template describe_metric
#'
#' @description `measure_std_mutualinfo()` assesses information loss by
#'   calculating the standardized mutual information for the target variables.
#'   See [mutual_information()].
#'
#' @template partition_step
#' @export
measure_std_mutualinfo <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables, drop = FALSE]

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

  rowmeans <- rowMeans(.x, na.rm = TRUE)
  long_means <- rep(rowmeans, ncols)

  within <- (as.numeric(.x) - long_means)^2
  among <- (long_means - mean(.x, na.rm = TRUE))^2

  ms1 <- sum(among, na.rm = TRUE) / (nrows - 1)
  ms2 <- sum(within, na.rm = TRUE) / (nrows * (ncols - 1))

  variance <- (ms1 - ms2) / ncols

  variance / (variance + ms2)
}


#' Count and retrieve the number of metrics below threshold
#'
#' @template partition_step_param
#' @rdname hits
#' @keywords internal
increase_hits <- function(.partition_step) {
  if (is.null(.partition_step$hits)) .partition_step$hits <- 0
  .partition_step$hits <- .partition_step$hits + 1

  .partition_step
}


#' @rdname hits
get_hits <- function(.partition_step) {
  if (is.null(.partition_step$hits)) return(0)
  .partition_step$hits
}

is_missing_rowwise <- function(.df) {
  as.logical(rowSums(is.na(.df)))
}

#' Process reduced variables when missing data
#'
#' @param x a vector, the reduced variable
#' @param .na a logical vector marking which are missing
#' @param .fill what to fill the missing locations with
#'
#' @return a vector of length nrow(original data)
#'
#' @return a character vector
#' @keywords internal
#' @rdname handle_missing
fill_in_missing <- function(x, .na, .fill = NA) {
  expanded_x <- vector("numeric", length = length(.na))
  expanded_x[!.na] <- x
  expanded_x[.na] <- .fill
  expanded_x
}

#' @rdname handle_missing
swap_nans <- function(.x) {
  nans <- is.nan(.x)
  if (any(nans)) .x <- fill_in_missing(.x[!nans], nans)
  .x
}

