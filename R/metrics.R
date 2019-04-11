metric_icc <- function(.partition_step) {
  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  .partition_step$metric <- icc_c(as.matrix(target_data))

  .partition_step
}

metric_min_icc <- function(.partition_step) {
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
  # for efficiency, this is actually handled in the pca function
  # so just return the partition step as is
  #
  # ...actually, maybe it should be here, then process if needed in reducer
  .partition_step
}
