#' @useDynLib partition, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#  variables used in various NSE calls
utils::globalVariables(
  c(
    ":=",
    "counts",
    "first",
    "groups",
    "indices",
    "information",
    "is_reduced",
    "mapping",
    "n",
    "name",
    "nclusters",
    "nreduced",
    "observed_info",
    "old_vars",
    "permutation",
    "position",
    "second",
    "target_info",
    "target_information",
    "variable"
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
