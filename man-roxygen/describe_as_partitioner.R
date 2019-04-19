#' @description Partitioners are functions that tell the partition algorithm 1)
#'   what to try to reduce 2) how to measure how much information is lost from
#'   the reduction and 3) how to reduce the data. In partition, functions that
#'   handle 1) are called directors, functions that handle 2) are called
#'   metrics,  and functions that handle 3) are called reducers. partition has a
#'   number of pre-specified partitioners for agglomerative data reduction.
#'   Custom partitioners can be created with [as_partitioner()].
#'
#' @md
