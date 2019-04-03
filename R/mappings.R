mappings <- function(.partition) {
  # return df of mappings
}

mapping_groups <- function(.partition, just_names = FALSE) {
  # return efficiently stored mappings (tibble with name of variable,
  # vector of contained vars (probably named vector for index e.g. "x1" = 1))
}

set_mappings <- function(.mappings) {
  # return partition_map, an empty partition obj
}

append_mappings <- function(.partition_step, new_x) {
  composite_variables <- pull_composite_variables(.partition_step)

  .partition_step$mapping_key %>%
    dplyr::filter(variable %nin% .partition_step$target) %>%
    dplyr::add_row(
      variable = new_x,
      mapping = list(composite_variables),
      information = .partition_step$metric
    )
}

expand_mappings <- function(x, .mapping_key) {
  .mapping_key %>%
    dplyr::filter(variable == x) %>%
    dplyr::select(mapping) %>%
    dplyr::pull() %>%
    purrr::pluck(1)
}

pull_composite_variables <- function(.partition_step) {
  purrr::map(
    .partition_step$target,
    expand_mappings,
    .partition_step$mapping_key
  ) %>%
    purrr::flatten_chr()
}

partition_scores <- function(.partition) {
 # return reduced data
}

replicate_partition <- function(new_data, .partition) {
  .mappings <- mapping_groups(.partition, just_names = TRUE)

  #  double check if names are in new data
  #  use variable positions if not
  same_names <- all(.mappings %in% names(new_data))
  if (!same_names) {
    warning("variable names do not match partition; using variable positions instead")
    .mappings <- mapping_groups(.partition)
  }

  # replicate parition exactly
}
