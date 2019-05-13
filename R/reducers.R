#' Create a custom reducer
#'
#' @template describe_reducer
#'
#' @param .f a function that returns either a numeric vector or a `data.frame`
#' @param ... Extra arguments passed to `.f`.
#' @param returns_vector logical. Does `.f` return a vector? `TRUE` by default.
#'   If `FALSE`, assumes that `.f` returns a `data.frame`.
#' @inheritParams reduce_cluster
#'
#' @return a function to use in [`as_partitioner()`]
#' @export
#'
#' @examples
#'
#' reduce_row_means <- as_reducer(rowMeans)
#' reduce_row_means
#'
#' @family reducers
as_reducer <- function(.f, ..., returns_vector = TRUE, first_match = NULL) {
  if (returns_vector) {
    mapping_f <- reduce_cluster
  } else {
    mapping_f <- map_cluster
  }

  if (!is.null(first_match)) {
    purrr::partial(mapping_f, first_match = first_match)
  }

  function(.partition_step, ...) {
    mapping_f(.partition_step, .f, ...)
  }
}

#' Reduce selected variables to scaled means
#'
#' @template describe_reducer
#'
#' @description `reduce_scaled_mean()` returns the scaled row means of the
#'   target variables to reduce.
#'
#' @template partition_step
#' @export
reduce_scaled_mean <- function(.partition_step) {
  reduce_cluster(.partition_step, scaled_mean_r)
}

#' Reduce selected variables to scaled means
#'
#' @template describe_reducer
#'
#' @description `reduce_kmeans()` is efficient in that it doesn't reduce until
#'   the closest `k` to the information threshold is found.
#'
#' @template partition_step
#' @param search  The search method. Binary search is generally more efficient
#'   but linear search can be faster in very low dimensions.
#' @param n_hits In linear search method, the number of iterations that should
#'   be under the threshold before reducing; useful for preventing false
#'   positives.
#' @export
reduce_kmeans <- function(.partition_step, search = c("binary", "linear"), n_hits = 4) {
  search <- match.arg(search)

  #  find next `k`
  if (search == "linear") return(linear_k_search(.partition_step, n_hits = n_hits))
  binary_k_search(.partition_step)
}

#' Reduce selected variables to first principal component
#'
#' @template describe_reducer
#'
#' @description `reduce_first_component()` returns the first component from the
#'   principal components analysis of the target variables.
#'
#' @template partition_step
#' @export
reduce_first_component <- function(.partition_step) {
  # this function uses the first PC, which is fit at the same time as variance
  # explained, so no need to pass a function. Just process it.
  reduce_cluster(.partition_step, NULL)
}

#' Reduce a target
#'
#' `reduce_cluster()` and `map_cluster()` apply the data reduction to the targets
#' found in the director step. They only do so if the metric is above the
#' threshold, however. `reduce_cluster()` is for functions that return vectors
#' while `map_cluster()` is for functions that return `data.frames`. If you're
#' using [`as_reducer()`], there's no need to call these functions directly.
#'
#' @template partition_step
#' @param .f a function to reduce the data to either a vector or a data.frame
#' @param rewind logical. Should the last target be used instead of the
#'   current target?
#' @param first_match logical. Should the partition algorithm stop when it finds
#'   a reduction that is equal to the threshold? Default is `TRUE` for reducers
#'   that return a `data.frame` and `FALSE` for reducers that return a vector
#'
#' @export
#'
#' @examples
#'
#' reduce_row_means <- function(.partition_step, .data) {
#'   reduce_cluster(.partition_step, rowMeans)
#' }
#'
#' replace_partitioner(
#'   part_icc,
#'   reduce = reduce_row_means
#' )
#'
#' @rdname reduce_target
reduce_cluster <- function(.partition_step, .f, first_match = FALSE) {
  #  if partitioning complete or threshold not met, skip reduce
  if (.partition_step$all_done) return(.partition_step)
  if (under_threshold(.partition_step)) return(.partition_step)

  #  if the variable is stored already, pull it. Otherwise calculate it.
  new_variable <- calculate_new_variable(.partition_step, .f)

  #  create new name and add `new_variable` to `reduced_data`
  new_x <- build_next_name(.partition_step)
  .partition_step$reduced_data <- .partition_step$reduced_data %>%
    dplyr::select(-!!.partition_step$target) %>%
    dplyr::mutate(!!new_x := new_variable)

  #  clean up the mapping key and add the new reduced variable
  .partition_step$mapping_key <- append_mappings(.partition_step, new_x = new_x)

  #  if there's a match with the tolerance and `first_match` is `TRUE`, then
  #  mark partitioning as complete
  exit_on_match <- first_match && metric_within_tolerance(.partition_step)
  if (exit_on_match) return(all_done(.partition_step))
  #  if there's only one column left, mark partitioning as complete
  if (all_columns_reduced(.partition_step)) return(all_done(.partition_step))

  .partition_step
}

#' @export
#' @rdname reduce_target
map_cluster <- function(.partition_step, .f, rewind = FALSE, first_match = FALSE) {
  #  if partitioning complete, skip reduce
  if (.partition_step$all_done) return(.partition_step)
  if (rewind) .partition_step <- rewind_target(.partition_step)

  #  create a list of the components of each cluster
  target_list <- purrr::map(
    sort(unique(.partition_step$target)),
    ~which(.partition_step$target == .x)
  )

  #  get the names of the component variables if needed
  named_targets <- all(is.character(target_list[[1]]))
  if (!named_targets) target_list <- get_names(.partition_step, target_list)

  #  reduce anything with more than one variable
  #  TODO: opportunity for parallelization
  #  Although this only gets called once in kmeans
  .partition_step$reduced_data <- purrr::map_dfc(
    target_list,
    ~return_if_single(.partition_step$.df[, .x, drop = FALSE], .f)
  )

  #  create the mapping key and name reduced variables in `reduced_data`
  .partition_step$mapping_key <- reduce_mappings(.partition_step, target_list)

  #  match mapping names to reduced variables in `reduced_data`
  df_names_map <- .partition_step$mapping_key %>%
    dplyr::mutate(mapping = purrr::map_chr(mapping, ~paste(.x, collapse = "_")))
  df_names <- tibble::tibble(
    name = names(.partition_step$reduced_data),
    mapping = purrr::map_chr(target_list, ~paste(.x, collapse = "_"))
  ) %>%
    dplyr::left_join(df_names_map, by = "mapping")
  #  set names to mapping key variable names
  names(.partition_step$reduced_data) <- df_names$variable
  #  reorder `reduced_data` by mapping key
  .partition_step$reduced_data <- .partition_step$reduced_data[, .partition_step$mapping_key$variable]

  #  if there's a match with the tolerance and `first_match` is `TRUE`, then
  #  mark partitioning as complete
  exit_on_match <- first_match && metric_within_tolerance(.partition_step)
  if (exit_on_match) return(all_done(.partition_step))
  #  if there's only one column left, mark partitioning as complete
  if (all_columns_reduced(.partition_step)) return(all_done(.partition_step))

  .partition_step
}

#' Search for the best `k`
#'
#' @template partition_step
#' @param search_method The search method. Binary search is generally more efficient
#'   but linear search can be faster in very low dimensions.
#'
#' @keywords internal
#' @importFrom stats median
search_k <- function(.partition_step, search_method = c("binary", "linear")) {
  search_method <- match.arg(search_method)

  #  linear search method: add or subtract 1
  if (search_method == "linear") {
    .partition_step$k <- .partition_step$k + .partition_step$k_search
    return(.partition_step)
  }

  #  binary search method: jump to median of target region
  #
  #  if this is is the first k, search the full range of the metric (0, 1)
  if (is.null(.partition_step$min_k)) .partition_step$min_k <- 1
  if (is.null(.partition_step$max_k)) .partition_step$max_k <- ncol(.partition_step$.df)

  #  find next k
  new_k <- ifelse(
    .partition_step$metric > .partition_step$threshold,
    #  if above threshold, search between minimum k and current k
    floor(median(.partition_step$min_k:.partition_step$k)),
    #  if below threshold, search between current k and maximum k
    ceiling(median(.partition_step$k:.partition_step$max_k))
  )

  if (.partition_step$metric > .partition_step$threshold) {
    #  if above threshold, set as new maximum
    .partition_step$max_k <- .partition_step$k
  } else {
    #  if above below, set as new minimum
    .partition_step$min_k <- .partition_step$k
  }

  .partition_step$k <- new_k

  .partition_step
}

#' Search for best `k` using the binary search method
#'
#' @template partition_step
#' @keywords internal
binary_k_search <- function(.partition_step) {
  # check if we've found the threshold boundary
  reduced_to_1 <- .partition_step$k == 1 && above_threshold(.partition_step)
  if (boundary_found(.partition_step) || reduced_to_1) {
    .partition_step <- map_cluster(.partition_step, scaled_mean_r, first_match = TRUE)
    return(all_done(.partition_step))
  }

  #  find the next k to assess
  .partition_step <- search_k(.partition_step, "binary")

  #  store last clusters, metrics, and k for later use
  .partition_step$last_target <- list(
    target = .partition_step$target,
    metric = .partition_step$metric_vector,
    k = .partition_step$k
  )

  .partition_step
}

#' Search for best `k` using the linear search method
#'
#' @template partition_step
#' @keywords internal
linear_k_search <- function(.partition_step, n_hits = 4) {
  if (is.null(.partition_step$k_search)) {
    #  if initial metric is smaller than threshold, search forward through k to
    #  capture more information. if it's larger, search backwards to minimize it
    smaller_than_threshold <- .partition_step$metric < .partition_step$threshold
    .partition_step$k_search <- ifelse(smaller_than_threshold, 1,-1)
  }

  # if we're searching forward, check if we've past the threshold
  if (k_searching_forward(.partition_step) && above_threshold(.partition_step)) {
    .partition_step <- map_cluster(.partition_step, scaled_mean_r, first_match = TRUE)
    return(all_done(.partition_step))
  }

  #   if we're searching backward, check if we've hit k = 1. Reduce if still above threshold
  #   and otherwise use k = 2
  if (k_searching_backward(.partition_step) && .partition_step$k == 1) {
    if (above_threshold(.partition_step)) {
      .partition_step <- map_cluster(.partition_step, scaled_mean_r, first_match = TRUE)
    } else {
      .partition_step <- rewind_target(.partition_step)
      .partition_step <- map_cluster(.partition_step, scaled_mean_r, first_match = TRUE)
    }
    return(all_done(.partition_step))
  }

  #   if we're searching backward, check if we've gone under the threshold. if
  #   so, use the last targets.
  if (k_searching_backward(.partition_step) && under_threshold(.partition_step) && get_hits(.partition_step) == n_hits)  {
    #  if none have been above boundary, don't reduce
    if (length(.partition_step$last_target) == 1 && is.na(.partition_step$last_target)) return(all_done(.partition_step))
    #  rewind to last target above boundary
    .partition_step <- rewind_target(.partition_step)
    .partition_step <- map_cluster(.partition_step, scaled_mean_r, first_match = TRUE)
    return(all_done(.partition_step))
  }

  #  find the next k to assess
  .partition_step <- search_k(.partition_step, "linear")

  .partition_step
}

#' Average and scale rows in a `data.frame`
#'
#' `scaled_mean()` calculates scaled row means for a dataframe.
#'
#' @param .x a `data.frame`
#' @param method The method source: both the pure R and C++ versions are efficient
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' library(dplyr)
#' iris %>%
#'   select_if(is.numeric) %>%
#'   scaled_mean()
#'
#' @rdname scaled_mean
scaled_mean <- function(.x, method = c("r", "c")) {
  method <- match.arg(method)
  if (method == "c") return(scaled_mean_c(.x))

  scaled_mean_r(.x)
}

scaled_mean_c <- function(.x) {
  .x <- scale_rowmeans(as.matrix(.x))
  swap_nans(.x)
}

scaled_mean_r <- function(.x) {
  .x <- as.numeric(scale(rowMeans(.x, na.rm = TRUE)))
  swap_nans(.x)
}

#' Create new variable name based on prefix and previous reductions
#'
#' @template partition_step_param
#'
#' @return a character vector
#' @keywords internal
build_next_name <- function(.partition_step) {
  reduced_names <- names(.partition_step$reduced_data)

  n_reduced_names <- reduced_names[length(reduced_names)] %>%
    stringr::str_split(.partition_step$var_prefix) %>%
    purrr::pluck(1) %>%
    # if null (no reduced variables), return 0
    purrr::pluck(2) %||% 0

  paste0(.partition_step$var_prefix, as.numeric(n_reduced_names) + 1)
}

#' Calculate or retrieve stored reduced variable
#'
#' @template partition_step_param
#'
#' @return a numeric vector, the reduced variable
#' @keywords internal
calculate_new_variable <- function(.partition_step, .f) {
  #  some methods calculate the metric and reduction at the same time. if a
  #  stored variable is present, use that.
  if (!is.null(.partition_step$new_variable)) {
    new_variable <- .partition_step$new_variable
    return(new_variable)
  }

  #  apply the reduction to the targeted variables
  .partition_step$.df %>%
    dplyr::select(pull_composite_variables(.partition_step)) %>%
    .f()
}

#' Reduce targets if more than one variable, return otherwise
#'
#' @param .x a `data.frame` containing the targets to reduce
#' @param .f a reduction function to apply
#' @param ... arguments passed to `.f`
#'
#' @return a numeric vector, the reduced or original variable
#' @keywords internal
return_if_single <- function(.x, .f, ...) {
  if (length(.x) == 1) return(unlist(.x, use.names = FALSE))
  .f(.x, ...)
}

#' Compare metric to threshold
#'
#' `under_threshold()` and `above_threshold()` check relative location of the
#' metric. `metric_within_tolerance()` uses `is_within()` to check if the metric
#' is within in the range of the threshold plus/minus the tolerance.
#'
#' @template partition_step_param
#'
#' @return logical, `TRUE` or `FALSE`
#' @keywords internal
#' @rdname compare_metric
under_threshold <- function(.partition_step) {
  .partition_step$metric < (.partition_step$threshold - .partition_step$tolerance)
}

#' @rdname compare_metric
above_threshold <- function(.partition_step) {
  .partition_step$metric > (.partition_step$threshold - .partition_step$tolerance)
}

#' @rdname compare_metric
is_within <- function(.x, .y, .e) {
  if (.e == 0) return(.x == .y)
  .x >= (.y - .e) && .x <= (.y + .e)
}

#' @rdname compare_metric
metric_within_tolerance <- function(.partition_step){
  is_within(.partition_step$metric, .partition_step$threshold, .partition_step$tolerance)
}

#' Set target to last value
#'
#' @template partition_step
#' @keywords internal
rewind_target <- function(.partition_step) {
  .partition_step$target <- .partition_step$last_target$target
  .partition_step$metric_vector <- .partition_step$last_target$metric
  .partition_step$metric <- min(.partition_step$metric_vector)
  .partition_step$k <- .partition_step$last_target$k

  .partition_step
}

#' Assess `k` search
#'
#' `k_searching_forward()` and `k_searching_backward()` check the direction of
#' the `k` search metric. `boundary_found()` checks if the last value of `k` was
#' under the threshold while the current value is over
#'
#' @template partition_step_param
#'
#' @return logical, `TRUE` or `FALSE`
#' @keywords internal
#' @rdname compare_k
k_searching_forward <- function(.partition_step) {
  .partition_step$k_search == 1
}

#' @rdname compare_k
k_searching_backward <- function(.partition_step) {
  .partition_step$k_search == -1
}

#' @rdname compare_k
boundary_found <- function(.partition_step) {
  k_above <- .partition_step$metric > .partition_step$threshold
  if (!is.null(.partition_step$metric_k1)) {
    k1_below <- .partition_step$metric_k1 < .partition_step$threshold
  } else {
    k1_below <- min(.partition_step$last_target$metric) < .partition_step$threshold
  }
  k_above && k1_below
}

#' Mark the partition as complete to stop search
#'
#' @template partition_step
#' @keywords internal
all_done <- function(.partition_step) {
  .partition_step$all_done <- TRUE
  .partition_step
}

#' Check if all variables reduced to a single composite
#'
#' @template partition_step_param
#' @return logical, `TRUE` or `FALSE`
#' @keywords internal
all_columns_reduced <- function(.partition_step) {
  ncol(.partition_step$reduced_data) == 1
}
