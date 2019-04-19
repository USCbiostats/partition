#' @export
print.partition <- function(x, ...) {
  #  methods used
  cat_bold("Partitioner:")
  cat("\n")
  print(x$partitioner)
  cat("\n\n")

  # number of clusters
  cat_bold("Number of Reduced Variables:")
  cat("\n")
  cat(
    crayon::green(count_clusters(x)),
    crayon::white("reduced variables created from"),
    crayon::yellow(total_reduced(x)),
    crayon::white("observed variables")
  )

  if (count_clusters(x) == 0) return(invisible(x))

  cat("\n\n")

  # summary of mapping
  cat_bold("Mappings:")
  cat("\n")
  cat(summarize_mapping(x))
  cat("\n\n")

  # summary of information
  cat_bold("Minimum information:")
  cat("\n")
  cat_white(minimum_information(x))

  # return partition object
  invisible(x)
}

#' @export
print.partitioner <- function(x, ...) {
  #  methods used
  cat_white(
    "  ",
    paste_director(x),
    " ",
    paste_metric(x),
    " ",
    paste_reducer(x)
  )

  invisible(x)
}

#' Print to the console in color
#'
#' @param ... text to print. Passed to `cat()` or `paste()`.
#'
#' @keywords internal
#' @rdname print_color
cat_bold <- function(...) cat(crayon::bold(...))

#' @rdname print_color
cat_white <- function(...) cat(crayon::white(...))

#' @rdname print_color
cat_subtle <- function(...) {
  cat(paste_subtle(...))
}

#' @rdname print_color
paste_subtle <- function(...) {
  paste0("\033[90m", ..., "\033[39m")
}

#' Helper functions to print `partition` summary
#'
#' @template partition_param
#' @param .round Should the minimum information be rounded?
#' @param digits If `.round` is `TRUE`, to what digit should it be rounded?
#' @keywords internal
#' @rdname print_helpers
count_clusters <- function(.partition) {
  reduced <- filter_reduced(.partition)
  nrow(reduced)
}

#' @rdname print_helpers
total_reduced <- function(.partition) {
  unnest_reduced(.partition) %>%
    nrow()
}

#' @rdname print_helpers
summarize_mapping <- function(.partition) {
  summary <- filter_reduced(.partition) %>%
    dplyr::mutate(
      old_vars = purrr::map_chr(mapping, ~paste(.x, collapse = ", ")),
      summary = paste0(
        crayon::green(variable),
        crayon::silver(" = {"),
        crayon::yellow(old_vars),
        crayon::silver("}")
      )
    )

    paste(summary$summary, collapse = "\n")
}

#' @rdname print_helpers
minimum_information <- function(.partition, .round = TRUE, digits = 3) {
  min_inf <- min(.partition$mapping_key$information)
  if (.round) min_inf <- round(min_inf, 3)
  min_inf
}

#' Lookup partitioner types to print in English
#'
#' @param x the function for which to find a description
#'
#' @return a description of the parts of the partitioner
#' @keywords internal
#' @rdname paste_partitioners
paste_director <- function(x) {
  director <- dplyr::case_when(
    is_same_function(x$direct, direct_distance_pearson) ~ "Minimum Distance (Pearson)",
    is_same_function(x$direct, direct_distance_spearman) ~ "Minimum Distance (Spearman)",
    is_same_function(x$direct, direct_k_cluster) ~ "K-Means Clusters",
    TRUE ~ paste_subtle("<custom director>")
  )

 paste("Director:", director, "\n")
}

#' @rdname paste_partitioners
paste_metric <- function(x) {
  metric <- dplyr::case_when(
    is_same_function(x$measure, metric_icc) ~ "Intraclass Correlation",
    is_same_function(x$measure, metric_min_icc) ~ "Minimum Intraclass Correlation",
    is_same_function(x$measure, metric_variance_explained) ~ "Variance Explained (PCA)",
    is_same_function(x$measure, metric_min_r2) ~ "Minimum R-Squared",
    is_same_function(x$measure, metric_std_mutualinfo) ~ "Standardized Mutual Information",
    TRUE ~ paste_subtle("<custom metric>")
  )

 paste("Metric:", metric, "\n")
}

#' @rdname paste_partitioners
paste_reducer <- function(x) {
  reducer <- dplyr::case_when(
    is_same_function(x$reduce, reduce_scaled_mean) ~ "Scaled Mean",
    is_same_function(x$reduce, reduce_kmeans) ~ "Scaled Mean",
    is_same_function(x$reduce, reduce_first_component) ~ "First Principal Component",
    TRUE ~ paste_subtle("<custom reducer>")
  )

 paste("Reducer:", reducer)
}

#' Are two functions the same?
#'
#' `is_same_function()` compares functions correctly even if they are partialized.
#'
#' @param x,y functions to compare
#'
#' @return logical: `TRUE` or `FALSE`
#' @keywords internal
is_same_function <- function(x, y) {
  # if arguments altered with purrr::partial(), get original function
  if (inherits(x, "purrr_function_partial")) x <- eval(attr(x, "fn"))

  identical(x, y)
}
