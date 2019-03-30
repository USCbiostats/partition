reduce_scaled_mean <- function(.partition_step) {
  # change this to reduce_data()? Not sure if generalizable
  .partition_step$reduced_data <- .partition_step$.df %>%
    dplyr::select(.partition_step$target) %>%
    scaled_mean_c()

  .partition_step
}

reduce_kmeans <- function(.df, indices) {

}

reduce_first_component <- function(.df, indices) {

}
