metric_icc <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  .partition_step$metric <- icc_r(as.matrix(target_data))

  .partition_step
}

metric_min_icc <- function(.partition_step, search_method = c("binary", "linear")) {
  search_method <- match.arg(search_method)
  if (.partition_step$all_done) return(.partition_step)

  # get indices for each cluster as list and subtract by one for cpp indexing
  indices <- purrr::map(
    seq_len(.partition_step$k),
    ~which(.partition_step$target == .x) - 1
  )

  k_icc <- min_icc_c(
    indices,
    as.matrix(.partition_step$.df),
    .partition_step$k,
    .partition_step$threshold
  )

  # use minimum icc as metric
  .partition_step$metric <- min(k_icc)
  # store vector of icc for mappings if min icc > threshold
  .partition_step$metric_vector <- k_icc

  if (search_method == "binary") {
    # also find the metrics for k - 1
    # get indices for each cluster as list and subtract by one for cpp indexing
    indices_k1 <- purrr::map(
      seq_len(.partition_step$k - 1),
      ~which(.partition_step$target_k1 == .x) - 1
    )

    k_icc_k1 <- min_icc_c(
      indices_k1,
      as.matrix(.partition_step$.df),
      .partition_step$k - 1,
      .partition_step$threshold
    )

    # use minimum icc as metric
    .partition_step$metric_k1 <- min(k_icc_k1)
    # store vector of icc for mappings if min icc > threshold
    .partition_step$metric_vector_k1 <- k_icc_k1
  }

  .partition_step
}

metric_variance_explained <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  pca1 <- pca_c(as.matrix(target_data))
  .partition_step$metric <- pca1[["pct_var"]]
  # PCA and variance explained are calculated at the same time for efficiency
  # so store the first PC to use later as the reduced variable
  uses_scaled_mean <- is_same_function(.partition_step$partition$reducer, reduce_scaled_mean)
  if (uses_scaled_mean) .partition_step$new_variable <- as.numeric(pca1[["pc1"]])

  .partition_step
}

metric_min_r2 <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  minr2 <- minR2_c(as.matrix(target_data))
  .partition_step$metric <- minr2[["minr2"]]
  # we need scaled means for min r2 calculation; store it to use for reducing
  uses_scaled_mean <- is_same_function(.partition_step$partition$reducer, reduce_scaled_mean)
  if (uses_scaled_mean) .partition_step$new_variable <- minr2[["row_means"]]

  .partition_step
}


metric_std_mutualinfo <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  mi <- mutual_information(target_data)
  .partition_step$metric <- mi[["standardized_mi"]]
  # we need scaled means for MI calculation; store it to use for reducing
  uses_scaled_mean <- is_same_function(.partition_step$partition$reducer, reduce_scaled_mean)
  if (uses_scaled_mean) .partition_step$new_variable <- mi[["scaled_row_means"]]

  .partition_step
}


icc_r <- function(.x) {
  ncols <- ncol(.x)
  nrows <- nrow(.x)

  rowmeans <- rowMeans(.x)
  long_means <- rep(rowmeans, ncols)

  within <- (as.numeric(.x) - long_means)^2
  among <- (long_means - mean(.x))^2

  ms1 <- sum(among) / (nrows - 1)
  ms2 <- sum(within) / (nrows * (ncols - 1))

  variance <- (ms1 - ms2) / ncols

  variance / (variance + ms2)
}
