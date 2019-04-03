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

as_partition_step <- function(.x, threshold = NA, reduced_data = NA, target = NA, metric = NA, ...) {
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
        mapping_key = mapping_key,
        all_done = FALSE,
        ...
      ),
      class = "partition_step"
    )
}

assign_partition <- function(.x, partitioner, .data, threshold, ...) {
  # i don't know if there's an early return mechanism
  # if (.x$threshold > threshold) return(purrr::done(.x))

  if (!is_partition_step(.x)) .x <- as_partition_step(.data)

  direct_measure_reduce(.x, partitioner, ...)
}

# this function may not be necessary as a wrapper unless doing more set up
# otherwise just call purrr::reduce directly in partition()
reduce_partition <- function(partitioner, .data, threshold, ..., niter) {

  reduced_obj <- purrr::reduce(
    seq_len(niter),
    assign_partition,
    .data = .data,
    ...,
    threshold = threshold
  )

  reduced_obj
}

as_partition <- function(partitioned_obj, partitioner) {
  # Scrub partition step
  #
  # * remove original data
  # * clean reduced names and mappings
  # *

  structure(
    partitioned_obj,
    partitioner,
    class = "partition"
  )
}

partition <- function(.data, partitioner, threshold, ..., niter = 1000) {
  partitioned_obj <- reduce_partition(
    .data,
    partitioner,
    threshold,
    ...,
    niter = niter
  )

  as_partition(partitioned_obj, partitioner)
}

is_partition <- function(x) inherits(x, "partition")

print.partition <- function(x, ...) {
  #  methods used

  # number of clusters

  # summary of mapping

  # summary of information

  # return partition object
  invisible(x)
}

print.partitioner <- function(x, ...) {
  #  methods used

  invisible(x)
}

