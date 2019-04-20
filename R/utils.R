#' @useDynLib partition2, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#  variables used in various NSE calls
utils::globalVariables(
  c(
    ":=",
    "indices",
    "is_reduced",
    "mapping",
    "old_vars",
    "variable",
    "nclusters",
    "nreduced",
    "observed_info",
    "permutation",
    "target_info"
  )
)

`%nin%` <- function(x, table) {
  #  is x not in table
  match(x, table, nomatch = 0L) == 0L
}

is_not_empty_or_na <- function(x) {
  if (length(x) > 1) return(!purrr::is_empty(x))

  !is.na(x)
}

`%||%` <- purrr::`%||%`
