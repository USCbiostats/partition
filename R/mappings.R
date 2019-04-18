#' Return partition mapping key
#'
#' `mappings()` returns a tidy data frame with each reduced variable and its
#' mapping and information loss. `mapping_groups()` returns a list of mappings
#' (either the variable names or their column position).
#'
#' @template partition_param
#' @param indices logical. Return just the indices instead of the names? Default is `FALSE`.
#'
#' @return a `tibble`
#' @export
#'
#' @examples
#'
#'
#' @rdname mapping_key
mappings <- function(.partition) {
  # return df of mappings
  tidyr::unnest(.partition$mapping_key)
}

#' @rdname mapping_key
mapping_groups <- function(.partition, indices = FALSE) {
  # return efficiently stored mappings (tibble with name of variable,
  # vector of contained vars (probably named vector for index e.g. "x1" = 1))
  if (indices) return(.partition$mapping_key$indices)
  .partition$mapping_key$mapping
}

set_mappings <- function(.mappings) {
  # return partition_map, an empty partition obj
}


replicate_partition <- function(new_data, .partition) {
  .mappings <- mapping_groups(.partition)

  #  double check if names are in new data
  #  use variable positions if not
  same_names <- all(purrr::flatten_chr(.mappings) %in% names(new_data))
  if (!same_names) {
    warning("variable names do not match partition; using variable positions")
    .mappings <- mapping_groups(.partition, indices = TRUE)
  }

  # replicate parition exactly
}


#' Filter the reduced mappings
#'
#' `filter_reduced()` and `unnest_reduced()` are convenience functions to
#' quickly retrieve the mappings for only the reduced variables.
#' `filter_reduced()` returns a nested `tibble` while `unnest_reduced()` unnests
#' it.
#'
#' @template partition_param
#'
#' @return a `tibble` with mapping key
#' @export
#'
#' @examples
#' @rdname filter_reduced
filter_reduced <- function(.partition) {
  .partition$mapping_key %>%
    dplyr::mutate(is_reduced = purrr::map_lgl(mapping, ~length(.x) > 1)) %>%
    dplyr::filter(is_reduced) %>%
    dplyr::select(-is_reduced)
}

#' @export
#' @rdname filter_reduced
unnest_reduced <- function(.partition) {
  .partition %>%
    filter_reduced() %>%
    tidyr::unnest()
}

#' Return the reduced data from a partition
#'
#' @template partition_param
#'
#' @return a tibble containing the reduced data for the partition
#' @export
#'
#' @examples
#' @rdname partition_scores
partition_scores <- function(.partition) {
 .partition$reduced_data
}

#' @export
#' @rdname partition_scores
fitted.partition <- partition_scores

#' Append a new variable to mapping and filter out composite variables
#'
#' @template partition_step_param
#' @param new_x the name of the reduced variable
#'
#' @return a `tibble`, the mapping key
#' @keywords internal
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

#' Create a mapping key out of a list of targets
#'
#' @template partition_step_param
#' @param target_list a list of composite variables
#'
#' @return a `tibble`, the mapping key
#' @keywords internal
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


#' Access mapping variables
#'
#' `pull_composite_variables()` takes a target and finds all the composite
#' variables (e.g. if a reduced variable is a target, it finds all the variables
#' the reduced variable is created from). `expand_mappings()` extracts the
#' composite variables of a given variable. `get_names()` finds the variable
#' names for a list of column positions.
#'
#' @template partition_step_param
#' @param .mapping_key a mapping key
#' @param target_list a list of composite variables
#'
#' @return a vector containing mappings
#' @keywords internal
#' @rdname pull_mappings
pull_composite_variables <- function(.partition_step) {
  purrr::map(
    .partition_step$target,
    expand_mappings,
    .partition_step$mapping_key
  ) %>%
    purrr::flatten_chr()
}

#' @rdname pull_mappings
expand_mappings <- function(x, .mapping_key) {
  .mapping_key[.mapping_key$variable == x, "mapping"][[1]][[1]]
}

#' @rdname pull_mappings
get_names <- function(.partition_step, target_list) {
  variable_names <- names(.partition_step$.df)
  purrr::map(target_list, ~variable_names[.x])
}

