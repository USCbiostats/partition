metric_icc <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  .partition_step$metric <- icc_c(as.matrix(target_data))

  .partition_step
}

metric_min_icc <- function(.partition_step) {
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
  .partition_step$metric_vector <- k_icc #TODO: CHANGE TO METRIC_VECTOR NOT IN LAST_TARGET, MAPPINGS TOO

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
  .partition_step$new_variable <- as.numeric(pca1[["pc1"]])

  .partition_step
}

metric_min_r2 <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  minr2 <- minR2_c(as.matrix(target_data))
  .partition_step$metric <- minr2[["minr2"]]
  # we need scaled means for min r2 calculation; store it to use for reducing
  .partition_step$new_variable <- minr2[["row_means"]]

  .partition_step
}


metric_std_mutualinfo <- function(.partition_step) {
  if (.partition_step$all_done) return(.partition_step)

  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  mi <- mutual_information(target_data)
  .partition_step$metric <- mi[["standardized_mi"]]
  # we need scaled means for MI calculation; store it to use for reducing
  .partition_step$new_variable <- mi[["scaled_row_means"]]

  .partition_step
}
