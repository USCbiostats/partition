cat_bold <- function(...) cat(crayon::bold(...))

cat_white <- function(...) cat(crayon::white(...))

cat_subtle <- function(...) {
  cat(paste_subtle(...))
}

paste_subtle <- function(...) {
  paste0("\033[90m", ..., "\033[39m")
}

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

paste_director <- function(x) {
  director <- dplyr::case_when(
    is_same_function(x$director, direct_distance_pearson) ~ "Miniumum Distance (Pearson)",
    is_same_function(x$director, direct_distance_spearman) ~ "Miniumum Distance (Spearman)",
    is_same_function(x$director, direct_k_cluster) ~ "K-Means Clusters",
    TRUE ~ paste_subtle("<custom director>")
  )

 paste("Director:", director, "\n")
}

paste_metric <- function(x) {
  metric <- dplyr::case_when(
    is_same_function(x$metric, metric_icc) ~ "Intraclass Correlation",
    is_same_function(x$metric, metric_min_icc) ~ "Minimum Intraclass Correlation",
    is_same_function(x$metric, metric_variance_explained) ~ "Variance Explained (PCA)",
    is_same_function(x$metric, metric_min_r2) ~ "Minimum R-Squared",
    is_same_function(x$metric, metric_std_mutualinfo) ~ "Standardized Mutual Information",
    TRUE ~ paste_subtle("<custom metric>")
  )

 paste("Metric:", metric, "\n")
}

paste_reducer <- function(x) {
  reducer <- dplyr::case_when(
    is_same_function(x$reducer, reduce_scaled_mean) ~ "Scaled Mean",
    is_same_function(x$reducer, reduce_kmeans) ~ "Scaled Mean",
    is_same_function(x$reducer, reduce_first_component) ~ "First Principal Component",
    TRUE ~ paste_subtle("<custom reducer>")
  )

 paste("Reducer:", reducer)
}

is_same_function <- function(x, y) {
  # if arguments altered with purrr::partial(), get original function
  if (inherits(x, "purrr_function_partial")) x <- eval(attr(x, "fn"))

  identical(x, y)
}
