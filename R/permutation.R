#' Permute a data set
#'
#' `permute_df()` permutes a data set: it randomizes the order within each
#' variable, which breaks any association between them. Permutation is useful
#' for testing against null statistics.
#'
#' @param .data a `data.frame`
#'
#' @return a permuted `data.frame`
#' @export
#'
#' @examples
#' permute_df(iris)
permute_df <- function(.data) {
  purrr::map_dfc(.data, ~.x[sample(x = seq_len(nrow(.data)), size = nrow(.data))])
}

#' Permute partitions
#'
#' `test_permutation()` permutes data and partitions the results to generate a
#' distribution of null statistics for observed information, number of clusters,
#' and number of observed variables reduced to clusters. The result is a
#' `tibble` with a summary of the observed data results and the averages of the
#' permuted results. The partitions and and permutations are also available in
#' `list-cols`. `test_permutation()` tests across a range of target information
#' values, as specified in the `information` argument.
#'
#' @param .data a data set to partition
#' @param information a vector of minimum information to fit in [partition()]
#' @param partitioner the partitioner to use. The default is [part_icc()].
#' @param ... arguments passed to [partition()]
#' @param nperm Number of permuted data sets to test. Default is 100.
#'
#' @return a tibble with summaries on observed and permuted data (the means of
#'   the permuted summaries), as well as list-cols containing them
#' @export
test_permutation <- function(.data, information = seq(.1, .6, by = .1), partitioner = part_icc(), ..., nperm = 100) {
  observed_summary <- map_partition(.data, partitioner = partitioner, information = information, ...)
  permutation_summary <- map_permutations(.data, partitioner = partitioner, information = information, ..., nperm = nperm)

  dplyr::left_join(observed_summary, permutation_summary, by = "target_info")
}


#' Map a partition across a range of minimum information
#'
#' `map_partition()` fits `partition()` across a range of minimum information
#' values, specified in the `information` argument. The output is a tibble with
#' a row for each value of `information`, a summary of the partition, and a
#' `list-col` containing the `partition` object.
#'
#' @inheritParams test_permutation
#'
#' @return a tibble
#' @export
#'
#' @examples
#' set.seed(123)
#' df <- simulate_block_data(c(3, 4, 5), lower_corr = .4, upper_corr = .6, n = 100)
#'
#' map_partition(df, partitioner = part_pc1())
#'
map_partition <- function(.data, partitioner = part_icc(), ..., information = seq(.1, .5, by = .1)) {
   partitions <- purrr::map(information, ~partition(.data, threshold = .x, partitioner = partitioner))
   purrr::map2_dfr(partitions, information, summarize_partitions)
}


#' Summarize and map partitions and permutations
#'
#' `summarize_partitions()` summarizes a `partition` and attaches it in a
#' `list-col`. `map_permutations()` processes [map_partition()] for a set of
#' permuted data sets.
#'
#' @inheritParams test_permutation
#' @return a tibble
#' @keywords internal
#' @rdname mapping_helpers
summarize_partitions <- function(.partition, .information) {
  tibble::tibble(
    target_info = .information,
    observed_info = minimum_information(.partition),
    nclusters = count_clusters(.partition),
    nreduced = total_reduced(.partition),
    partition = list(.partition)
  )
}

#' @rdname mapping_helpers
map_permutations <- function(.data, partitioner = part_icc(), ..., information = seq(.1, .5, by = .1), nperm = 100) {
  permuted_data <- purrr::rerun(nperm, permute_df(.data))
  permuted_summaries <- purrr::map_dfr(
    permuted_data,
    map_partition,
    partitioner = partitioner,
    ...,
    information = information,
    .id = ".permutation"
  ) %>%
    dplyr::group_by(target_info)

  permuted_summary <- permuted_summaries %>%
    dplyr::summarise(
      perm_observed_info = mean(observed_info),
      perm_nclusters = mean(nclusters),
      perm_nreduced = mean(nreduced)
    )

  permutation_tbls <- dplyr::group_nest(permuted_summaries, .key = "permutation", keep = TRUE)
  dplyr::left_join(permuted_summary, permutation_tbls, by = "target_info")
}
