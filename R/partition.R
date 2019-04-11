as_partitioner <- function(reducer, director, metric) {
  structure(
    list(
      reducer = reducer,
      director = director,
      metric = metric
    ),
    class = "partitioner"
  )
}

direct_measure_reduce <- function(.x, partitioner) {
  .x %>%
    partitioner$director() %>%
    partitioner$metric() %>%
    partitioner$reducer()
}

is_partition_step <- function(x) inherits(x, "partition_step")

as_partition_step <- function(.x, threshold = NA, reduced_data = NA, target = NA, metric = NA, tolerance = .01, var_prefix = NA, ...) {
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
        ...
      ),
      class = "partition_step"
    )
}

assign_partition <- function(.x, partitioner, .data, threshold, tolerance, var_prefix, ...) {

if (!is_partition_step(.x)) {
  .x <- as_partition_step(
    .data,
    threshold = threshold,
    tolerance = tolerance,
    var_prefix = var_prefix
  )
}

  direct_measure_reduce(.x, partitioner, ...)
}

# # this function may not be necessary as a wrapper unless doing more set up
# # otherwise just call purrr::reduce directly in partition()
# reduce_partition <- function(partitioner, .data, threshold, ..., niter) {
#
#   reduced_obj <- purrr::reduce(
#     seq_len(niter),
#     assign_partition,
#     .data = .data,
#     ...,
#     threshold = threshold
#   )
#
#   reduced_obj
# }

as_partition <- function(partitioned_obj, partitioner) {
  # Scrub partition step:
  #
  # * remove original data
  # * clean reduced names and mappings
  partitioned_obj <- simplify_names(partitioned_obj)

  structure(
    list(
      reduced_data = partitioned_obj$reduced_data,
      mapping_key = partitioned_obj$mapping_key,
      threshold = partitioned_obj$threshold,
      partitioner = partitioner
    ),
    class = "partition"
  )
}

#import `!!`, `!!!`
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

partition <- function(.data, partitioner, threshold, tolerance = .01, ..., niter = 1000, x = "reduced_var", .sep = "_") {
  partitioned_obj <- reduce_partition_c(
    .data,
    df = .data,
    assign_partition = assign_partition,
    partitioner = partitioner,
    threshold = threshold,
    tolerance = tolerance,
    var_prefix = paste0(x, .sep),
    # currently unused
    ...,
    niter = niter
  )

  as_partition(partitioned_obj, partitioner)
}

is_partition <- function(x) inherits(x, "partition")

