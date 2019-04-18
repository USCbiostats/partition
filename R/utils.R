#' @useDynLib partition2, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

`%nin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

is_not_empty_or_na <- function(x) {
  if (length(x) > 1) return(!purrr::is_empty(x))

  !is.na(x)
}

`%||%` <- purrr::`%||%`
