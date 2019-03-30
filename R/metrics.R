metric_icc <- function() {
  icc_c()
}

metric_variance_explained <- function(.partition_step) {
  # for efficiency, this is actually handled in the pca function
  # so just return the partition step as is
  .partition_step
}

