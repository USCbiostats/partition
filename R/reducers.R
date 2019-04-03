reduce_scaled_mean <- function(.partition_step) {
  # change this to reduce_data()? Not sure if generalizable
  reduce_data(.partition_step, scaled_mean_c)
}

reduce_kmeans <- function(.df, indices) {

}

reduce_first_component <- function(.df, indices) {

}

scaled_mean_c <- function(.x) {
  scale_rowmeans(as.matrix(.x))
}

build_next_name <- function(.partition_step, x = "reduced_var", .sep = "_") {
  reduced_names <- names(.partition_step$reduced_data)

  n_reduced_names <- reduced_names[length(reduced_names)] %>%
    stringr::str_split(paste0(x, .sep)) %>%
    purrr::pluck(1) %>%
    # if null (no reduced variables), return 0
    purrr::pluck(2) %||% 0

  paste0(x, .sep, as.numeric(n_reduced_names) + 1)
}

reduce_data <- function(.partition_step, .f) {
  if (under_threshold(.partition_step)) return(.partition_step)

  new_variable <- .partition_step$reduced_data %>%
    dplyr::select(.partition_step$target) %>%
    .f()

  new_x <- build_next_name(.partition_step)
  .partition_step$reduced_data <- .partition_step$reduced_data %>%
    dplyr::select(-!!.partition_step$target) %>%
    dplyr::mutate(!!new_x := new_variable)

  .partition_step$mapping_key <- append_mappings(.partition_step, new_x = new_x)

  .partition_step
}

under_threshold <- function(.partition_step) .partition_step$metric <= .partition_step$threshold

part_mean_icc <- as_partitioner(
  reducer = reduce_scaled_mean,
  metric = metric_icc,
  director = direct_distance_pearson
)
