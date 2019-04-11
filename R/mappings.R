mappings <- function(.partition) {
  # return df of mappings
  tidr::unnest(.partition$mapping_key)
}

mapping_groups <- function(.partition, just_names = FALSE) {
  # return efficiently stored mappings (tibble with name of variable,
  # vector of contained vars (probably named vector for index e.g. "x1" = 1))
  .partition$mapping_key
}

set_mappings <- function(.mappings) {
  # return partition_map, an empty partition obj
}

# export this
filter_reduced <- function(.partition) {
  .partition$mapping_key %>%
    dplyr::mutate(is_reduced = purrr::map_lgl(mapping, ~length(.x) > 1)) %>%
    dplyr::filter(is_reduced) %>%
    dplyr::select(-is_reduced)
}

count_clusters <- function(.partition) {
  reduced <- filter_reduced(.partition)
  nrow(reduced)
}

total_reduced <- function(.partition) {
  filter_reduced(.partition) %>%
    tidyr::unnest() %>%
    nrow()
}

summarize_mapping <- function(.partition) {
  summary <- filter_reduced(.partition) %>%
    dplyr::mutate(
      old_vars = purrr::map_chr(mapping, ~paste(.x, collapse = ", ")),
      summary = paste0(
        crayon::green(variable),
        crayon::silver(" = {"),
        crayon::yellow(old_vars),
        crayon::silver("}")
      )
    )

  paste(summary$summary, collapse = "\n")
}

minimum_information <- function(.partition, .round = TRUE, digits = 3) {
  min_inf <- min(.partition$mapping_key$information)
  if (.round) min_inf <- round(min_inf, 3)
  min_inf
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

reduce_mappings <- function(.partition_step, target_list) {
  named_targets <- all(is.character(target_list[[1]]))
  if (!named_targets) target_list <- get_names(.partition_step, target_list)

  tibble::tibble(
    variable = paste0(.partition_step$var_prefix, seq_along(target_list)),
    mapping = target_list,
    information = .partition_step$metric_vector
  ) %>%
    dplyr::mutate(variable = purrr::map2_chr(
      mapping,
      variable,
      # if there's only a single variable,
      # call it by it's name rather than
      # the reduced variable name
      ~ ifelse(length(.x) > 1, .y, .x[[1]])
    ))
}

expand_mappings <- function(x, .mapping_key) {
  .mapping_key[.mapping_key$variable == x, "mapping"][[1]][[1]]
}

pull_composite_variables <- function(.partition_step) {
  purrr::map(
    .partition_step$target,
    expand_mappings,
    .partition_step$mapping_key
  ) %>%
    purrr::flatten_chr()
}

get_names <- function(.partition_step, target_list) {
  variable_names <- names(.partition_step$.df)
  purrr::map(target_list, ~variable_names[.x])
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
