#' Return partition mapping key
#'
#' `mapping_key()` returns a data frame with each reduced variable and its
#' mapping and information loss; the mapping and indices are represented as
#' `list-cols` (so there is one row per variable in the reduced data set).
#' `unnest_mappings()` unnests the list columns to return a tidy data frame.
#' `mapping_groups()` returns a list of mappings (either the variable names or
#' their column position).
#'
#' @template partition_param
#' @param indices logical. Return just the indices instead of the names? Default is `FALSE`.
#'
#' @return a `tibble`
#' @export
#'
#' @template example_header
#' @examples
#' # tibble: 6 x 4
#' mapping_key(prt)
#'
#' # tibble: 12 x 4
#' unnest_mappings(prt)
#'
#' # list: length 6
#' mapping_groups(prt)
#'
#' @rdname mapping_key
mapping_key <- function(.partition) {
  # return mapping key
  .partition$mapping_key
}

#' @export
#' @rdname mapping_key
unnest_mappings <- function(.partition) {
  # return unnested df of mappings
  tidyr::unnest(.partition$mapping_key, cols = c(mapping, indices))
}

#' @export
#' @rdname mapping_key
mapping_groups <- function(.partition, indices = FALSE) {
  # return list of variable names or indices in mapping
  if (indices) return(.partition$mapping_key$indices)
  .partition$mapping_key$mapping
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
#' @template example_header
#' @examples
#'
#' # A tibble: 3 x 4
#' filter_reduced(prt)
#'
#' # A tibble: 9 x 4
#' unnest_reduced(prt)
#'
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
    tidyr::unnest(cols = c(mapping, indices))
}

#' Return the reduced data from a partition
#'
#' The reduced data is stored as `reduced_data` in the partition object and can
#' thus be returned by subsetting `object$reduced_data`. Alternatively, the
#' functions `partition_score()` and `fitted()` also return the reduced data.
#'
#' @param object a `partition` object
#' @param ... not currently used (for S3 consistency with `fitted()`)
#'
#' @return a tibble containing the reduced data for the partition
#' @export
#'
#' @template example_header
#' @examples
#'
#' # three ways to retrieve reduced data
#' partition_scores(prt)
#' fitted(prt)
#' prt$reduced_data
#'
#' @rdname partition_scores
partition_scores <- function(object, ...) {
  object$reduced_data
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


  mapping_key <- tibble::tibble(
    mapping = target_list,
    information = .partition_step$metric_vector
  ) %>%
    dplyr::arrange(information) %>%
    dplyr::mutate(
      variable = paste0(.partition_step$var_prefix, seq_along(target_list)),
      variable = purrr::map2_chr(
        mapping,
        variable,
        # if there's only a single variable, call it by its name
        ~ ifelse(length(.x) > 1, .y, .x[[1]])
      ))

  # sort by position in data and information
  tibble::tibble(
    variable = names(.partition_step[[".df"]]),
    position = seq_along(variable)
  ) %>%
    dplyr::right_join(mapping_key, by = "variable") %>%
    dplyr::arrange(position, variable) %>%
    dplyr::select(-position) %>%
    dplyr::arrange(information)
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

