#' Simulate correlated blocks of variables
#'
#' `simulate_block_data()` creates a dataset of blocks of data where variables
#' within each block are correlated. The correlation for each pair of variables
#' is sampled uniformly from `lower_corr` to `upper_corr`, and the values of
#' each are sampled using [MASS::mvrnorm()].
#'
#' @param block_sizes a vector of block sizes. The size of each block is the
#'   number of variables within it.
#' @param lower_corr the lower bound of the correlation within each block
#' @param upper_corr the upper bound of the correlation within each block
#' @param n the number of observations or rows
#' @param block_name description prepended to the variable to indicate the block it belongs to
#' @param sep a character, what to separate the variable names with
#' @param var_name the name of the variable within the block
#'
#' @return a `tibble` with `sum(block_sizes)` columns and `n` rows.
#' @export
#'
#' @examples
#' # create a 100 x 15 data set with 3 blocks
#' simulate_block_data(
#'   block_sizes = rep(5, 3),
#'   lower_corr = .4,
#'   upper_corr = .6,
#'   n = 100
#' )
#' @importFrom stats runif
simulate_block_data <- function(block_sizes, lower_corr, upper_corr, n, block_name = "block",
                             sep = "_", var_name = "x") {
  #  simulate length(block_sizes) blocks and bind them in a data.frame
	sim_data <- purrr::map(
	  block_sizes,
	  simulate_block,
	  n = n,
	  lower_corr = lower_corr,
	  upper_corr = upper_corr
	) %>%
	  dplyr::bind_cols(.name_repair = "minimal")

	#  create column names: `block_name`+`sep`+`var_name`+n
	col_names <- purrr::map2(
	  block_sizes,
	  seq_along(block_sizes),
	  ~rep(paste0(block_name, .y), .x)
  ) %>%
  purrr::map(~paste0(.x, sep, var_name, seq_along(.x)))
	names(sim_data) <- purrr::flatten_chr(col_names)

	sim_data
}

simulate_block <- function(block_n, n, lower_corr, upper_corr) {
  #  specify a covariance matrix of the variables by pulling correlation from
  #  uniform distribution
  sigma <- matrix(
    runif(block_n^2, lower_corr, upper_corr),
    ncol = block_n,
    nrow = block_n
  )
  sigma[lower.tri(sigma)] <- t(sigma)[lower.tri(sigma)]
  diag(sigma) <- 1

  #  simulate block of multivariate normal variables
  block <- MASS::mvrnorm(n, mu = rep(0, block_n), Sigma = sigma)

  tibble::as_tibble(data.frame(block))
}
