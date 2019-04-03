  #' @useDynLib partition2, .registration = TRUE
  #' @importFrom Rcpp sourceCpp
  NULL

  corr <- function(x, y = NULL, spearman = FALSE) {
    if (is.null(y)) {
      dim_names <- names(x)
      if (spearman) {
        x <- apply_rank(as.matrix(x))
      }

      correlation <- corr_c_mat(as.matrix(x))

      attr(correlation, "dimnames") <- list(dim_names, dim_names)
      return(correlation)
    }

    if (spearman) {
      x <- rank_c(x)
      y <- rank_c(y)
    }

    corr_c_2vec(x, y)
  }

`%nin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}

is_not_empty_or_na <- function(x) {
  if (length(x) > 1) return(!purrr::is_empty(x))

  !is.na(x)
}

`%||%` <- purrr::`%||%`
