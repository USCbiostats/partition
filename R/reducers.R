reduce_scaled_mean <- function(.partition_step) {
  # change this to reduce_data()? Not sure if generalizable
  reduce_data(.partition_step, scaled_mean_c)
}

reduce_kmeans <- function(.partition_step) {
  if (is.null(.partition_step$k_search)) {
    #  if initial metric is smaller than threshold, search forward through k to
    #  capture more information. if it's larger, search backwards to minimize it
    smaller_than_threshold <- .partition_step$metric < .partition_step$threshold
    .partition_step$k_search <- ifelse(smaller_than_threshold, 1,-1)
  }

  # if we're searching forward, check if we've past the threshold
  if (k_searching_forward(.partition_step) && above_threshold(.partition_step)) {
    .partition_step <- map_data(.partition_step, scaled_mean_c)
    .partition_step$all_done <- TRUE
    return(.partition_step)
  }

  # if we're searching backward, check if we've gone under the threshold. if so,
  # use the last targets.
  if (k_searching_backward(.partition_step) && under_threshold(.partition_step)) {
    .partition_step$target <- .partition_step$last_target$target
    .partition_step$metric_vector <- .partition_step$last_target$metric_vector
    .partition_step$metric <- min(.partition_step$metric)
    .partition_step$k <- .partition_step$k - 1
    .partition_step <- map_data(.partition_step, scaled_mean_c)
    .partition_step$all_done <- TRUE
    return(.partition_step)
  }

  .partition_step <- map_data(.partition_step, scaled_mean_c)
  .partition_step$k <- search_k(.partition_step)
  .partition_step$last_target <- list(
    target = .partition_step$target,
    metric = .partition_step$metric_vector,
    k = .partition_step$k
  )

  .partition_step
}

reduce_first_component <- function(.df, indices) {

}

scaled_mean_c <- function(.x) {
  scale_rowmeans(as.matrix(.x))
}

build_next_name <- function(.partition_step) {
  reduced_names <- names(.partition_step$reduced_data)

  n_reduced_names <- reduced_names[length(reduced_names)] %>%
    stringr::str_split(.partition_step$var_prefix) %>%
    purrr::pluck(1) %>%
    # if null (no reduced variables), return 0
    purrr::pluck(2) %||% 0

  paste0(.partition_step$var_prefix, as.numeric(n_reduced_names) + 1)
}

#  for reducers that return a vector
reduce_data <- function(.partition_step, .f) {
  if (.partition_step$all_done) return(.partition_step)
  if (under_threshold(.partition_step)) return(.partition_step)

  new_variable <- .partition_step$reduced_data %>%
    dplyr::select(.partition_step$target) %>%
    .f()

  new_x <- build_next_name(.partition_step)
  .partition_step$reduced_data <- .partition_step$reduced_data %>%
    dplyr::select(-!!.partition_step$target) %>%
    dplyr::mutate(!!new_x := new_variable)

  .partition_step$mapping_key <- append_mappings(.partition_step, new_x = new_x)

  if (metric_within_tolerance(.partition_step)) .partition_step$all_done <- TRUE

  .partition_step
}

return_if_single <- function(.x, .f, ...) {
  if (length(.x) == 1) return(unlist(.x, use.names = FALSE))
  .f(.x, ...)
}

#  for reducers that return a data frame
map_data <- function(.partition_step, .f) {
  if (.partition_step$all_done) return(.partition_step)
  if (under_threshold(.partition_step)) return(.partition_step)

  target_list <- purrr::map(
    seq_len(.partition_step$k),
    ~which(.partition_step$target == .x)
  )

  named_targets <- all(is.character(target_list[[1]]))
  if (!named_targets) target_list <- get_names(.partition_step, target_list)

  .partition_step$reduced_data <- purrr:::map_dfc(
    target_list,
    ~return_if_single(.partition_step$.df[, .x], .f)
  )

  .partition_step$mapping_key <- reduce_mappings(.partition_step, target_list)
  names(.partition_step$reduced_data) <- .partition_step$mapping_key$variable

  if (metric_within_tolerance(.partition_step)) .partition_step$all_done <- TRUE

  .partition_step
}

under_threshold <- function(.partition_step) {
  .partition_step$metric < (.partition_step$threshold - .partition_step$tolerance)
}

above_threshold <- function(.partition_step) {
  .partition_step$metric > (.partition_step$threshold + .partition_step$tolerance)
}

k_searching_forward <- function(.partition_step) {
  .partition_step$k_search == 1
}

k_searching_backward <- function(.partition_step) {
  .partition_step$k_search == -1
}

is_within <- function(.x, .y, .e) {
  if (.e == 0) return(.x == .y)
  .x >= (.y - .e) && .x <= (.y + .e)
}

metric_within_tolerance <- function(.partition_step){
  is_within(.partition_step$metric, .partition_step$threshold, .partition_step$tolerance)
}

search_k <- function(.partition_step) {
  .partition_step$k + .partition_step$k_search
}

