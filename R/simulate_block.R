simulate_block_data <- function(block_sizes, lower_corr, upper_corr, n, block_name = "block",
                             sep = "_", var_name = "x") {

	sim_data <- purrr::map_dfc(
	  block_sizes,
	  simulate_block,
	  n = n,
	  lower_corr = lower_corr,
	  upper_corr = upper_corr
	)

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
  sigma <- matrix(
    runif(block_n^2, lower_corr, upper_corr),
    ncol = block_n,
    nrow = block_n
  )
  sigma[lower.tri(sigma)] <- t(sigma)[lower.tri(sigma)]
  diag(sigma) <- 1

  block <- MASS::mvrnorm(n, mu = rep(0, block_n), Sigma = sigma)
  tibble::as_tibble(data.frame(block))
}
