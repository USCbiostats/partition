
metric_icc <- function(.partition_step) {
  composite_variables <- pull_composite_variables(.partition_step)
  target_data <- .partition_step$.df[, composite_variables]

  .partition_step$metric <- icc_c(as.matrix(target_data))

  .partition_step
}

metric_variance_explained <- function(.partition_step) {
  # for efficiency, this is actually handled in the pca function
  # so just return the partition step as is
  .partition_step
}

store_mapping <- function(.partition_step, x, metric) {
  .partition_step$metric_key
  .partition_step$group_key

  .partition_step
}
