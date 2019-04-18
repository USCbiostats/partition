#' Agglomerative partitioning dimension reduction
#'
#'
#'
#' @param .data a data.frame to partition
#' @param threshold the minimum information loss acceptable
#' @param partitioner a `partitioner`. See the `part_*()` functions and
#'   [`as_partitioner()`].
#' @param tolerance a small tolerance within the threshold; if a reduction is
#'   within the threshold plus/minus the tolerance, it will reduce.
#' @param niter the number of iterations. By default, it is calculated as 20% of
#'   the number of variables or 10, whichever is larger.
#' @param x the prefix of the new variable names
#' @param .sep a character vector that separates `x` from the number (e.g. "reduced_var_1").
#'
#' @return a `partition` object
#' @export
#'
#' @examples
partition <- function(.data, threshold, partitioner = part_icc(), tolerance = .01, niter = NULL, x = "reduced_var", .sep = "_") {
  # set number of unsuccesful iterations allowed in a row to be ~20% of the
  # number of variables but at least 10
  if (is.null(niter)) {
    niter <- round(.2 * ncol(.data))
    niter <- ifelse(niter < 10, 10, niter)
  }

  # iteratively reduce data using partition algorithm
  partitioned_obj <- reduce_partition_c(
    .data,
    df = .data,
    assign_partition = assign_partition,
    partitioner = partitioner,
    threshold = threshold,
    tolerance = tolerance,
    var_prefix = paste0(x, .sep),
    niter = niter
  )

  # return as `partition` object
  as_partition(partitioned_obj)
}

#' Apply a partitioner
#'
#' `direct_measure_reduce()` works through the direct, measure, reduce steps of
#' the partition algorithm, applying the `partitioner` to the `partition_step`.
#'
#' @template partition_step
#' @param partitioner a partitioner, as created from [as_partitioner()].
#'
#' @keywords internal
#' @seealso [as_partitioner()]
direct_measure_reduce <- function(.partition_step, partitioner) {
  .partition_step %>%
    partitioner$direct() %>%
    partitioner$measure() %>%
    partitioner$reduce()
}

#' Is this object a `partition_step`?
#'
#' @param x an object to be tested
#'
#' @return logical: `TRUE` or `FALSE`
#' @export
is_partition_step <- function(x) inherits(x, "partition_step")

#' Create a partition object from a data frame
#'
#' `as_partition_step()` creates a `partition_step` object. `partition_step`s
#' are used while iterating through the partition algorithm: it stores necessary
#' information about how to proceed in the partitioning, such as the information
#' threshold. `as_partition_step()` is primarily called internally by
#' `partition()` but can be helpful while developing `partitioners`.
#'
#' @param .x a `data.frame` or `partition_step` object
#' @param threshold The minimum information loss allowable
#' @param reduced_data A data set with reduced variables
#' @param target A character or integer vector: the variables to reduce
#' @param metric A measure of information
#' @param tolerance A tolerance around the threshold to accept a reduction
#' @param var_prefix Variable name for reduced variables
#' @param partitioner A `partitioner`, a `part_*()` function or one created with
#'   [as_partitioner()].
#' @param ... Other objects to store during the partition step
#'
#' @return a `partition_step` object
#' @export
#'
#' @examples
#' .df <- data.frame(x = rnorm(100), y = rnorm(100))
#' as_partition_step(.df, threshold = .6)
as_partition_step <- function(.x, threshold = NA, reduced_data = NA, target = NA, metric = NA, tolerance = .01, var_prefix = NA, partitioner = NA, ...) {
  # do I want to swap reduced_data for .df here? or do I need the full data? update_partition_step()
  if (is_partition_step(.x)) return(.x)

  mapping_key <- tibble::tibble(
    variable = names(.x),
    mapping = purrr::map(names(.x), ~.x),
    information = 1.0
  )

  if (!is_not_empty_or_na(reduced_data)) reduced_data <- .x

  structure(
      list(
        .df = .x,
        threshold = threshold,
        target = target,
        last_target = NA,
        reduced_data = reduced_data,
        metric = metric,
        tolerance = tolerance,
        mapping_key = mapping_key,
        var_prefix = var_prefix,
        all_done = FALSE,
        partitioner = partitioner,
        ...
      ),
      class = "partition_step"
    )
}

#' Process a dataset with a partitioner
#'
#' `assign_partition()` is the primary handler for the partition algorithm and
#' is iterated by `reduce_partition_c()`. `assign_partition()` does initial set
#' up of the `partition_step` object and then applies the `partitioner` to each
#' iteration of the  `partition_step` via `direct_measure_reduce()`.
#'
#' @param .x the data or a `partition_step` object
#' @inheritParams partition
#'
#' @return a `partition_step` object
#' @keywords internal
assign_partition <- function(.x, partitioner, .data, threshold, tolerance, var_prefix) {

  #  for the first iteration, create a `partition_step` object to help
  #  process the partition
  if (!is_partition_step(.x)) {
    .x <- as_partition_step(
      .data,
      threshold = threshold,
      tolerance = tolerance,
      var_prefix = var_prefix,
      partitioner = partitioner
    )
  }

  #  apply the partitioner to the data
  direct_measure_reduce(.x, partitioner)
}

#' Return a partition object
#'
#' `as_partition()` is called when partitioning is complete. It scrubs a
#' `partition_step` object, cleans the reduced variable names, adds mapping
#' indices, and sorts the composite variables.
#'
#' @template partition_step_param
#'
#' @return a `partition` object
#' @keywords internal
#'
#' @examples
as_partition <- function(partition_step) {
  # Scrub partition_step:
  #
  # * clean reduced names and mappings
  partition_step <- simplify_names(partition_step)
  # * add variable positions (indices) to mapping
  partition_step <- add_indices(partition_step)
  # * sort mappings by order in original data
  partition_step <- sort_mapping(partition_step)

  structure(
    list(
      reduced_data = partition_step$reduced_data,
      mapping_key = partition_step$mapping_key,
      threshold = partition_step$threshold,
      partitioner = partition_step$partitioner
    ),
    class = "partition"
  )
}

#' Simplify reduced variable names
#'
#' @template partition_step
#' @keywords internal
#'
#' @importFrom rlang !! !!!
simplify_names <- function(.partition_step) {
  #  get reduced variable names
  var_names <- .partition_step$mapping_key %>%
    dplyr::filter(purrr::map_lgl(mapping, ~ length(.x) > 1)) %>%
    dplyr::pull(variable)

  #  return if no data reduction happened
  if (purrr::is_empty(var_names)) return(.partition_step)

  #  create new names
  updated_var_names <- paste0(.partition_step$var_prefix, seq_along(var_names))
  names(var_names) <- updated_var_names
  names(updated_var_names) <- var_names

  #  rename columns
  .partition_step$reduced_data <- dplyr::rename(.partition_step$reduced_data, !!!var_names)
  #  rename mappings
  .partition_step$mapping_key <- .partition_step$mapping_key %>%
    dplyr::mutate(variable = dplyr::recode(variable, !!!updated_var_names))

  .partition_step
}

#' Process mapping key to return from `partition()`
#'
#' `add_indices()` uses `get_indices()` to add the variable positions to the
#' mapping key. `sort_mapping()` sorts the composite variables of each reduced
#' variable by their position in the original data.
#'
#' @template partition_step
#' @keywords internal
#'
#' @rdname as_partition_helpers
get_indices <- function(.partition_step) {
  .partition_step$mapping_key$mapping %>%
    # take list of mappings
    purrr::map(
      # in each element is a vector of variable names
      # get the
      ~ which(names(.partition_step$.df) %in% .x)
    )
}

#' @rdname as_partition_helpers
add_indices <- function(.partition_step) {
   .partition_step$mapping_key <- .partition_step$mapping_key %>%
     dplyr::mutate(indices = get_indices(.partition_step))

  .partition_step
}

#' @rdname as_partition_helpers
sort_mapping <- function(.partition_step) {
  #  sort the varible names by their position in the original data
  .partition_step$mapping_key <- .partition_step$mapping_key %>%
    dplyr::mutate(mapping = purrr::map(
      indices,
      ~names(.partition_step$.df)[.x]
    ))

  .partition_step
}

#' Is this object a partition?
#'
#' @param x an object to be tested
#'
#' @return logical: `TRUE` or `FALSE`
#' @export
is_partition <- function(x) inherits(x, "partition")
