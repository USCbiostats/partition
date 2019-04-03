direct_distance_pearson <- function(.partition) {
  direct_distance(.partition, spearman = FALSE)
}

direct_distance_spearman <- function(.partition) {
  direct_distance(.partition, spearman = TRUE)
}

direct_distance <- function(.partition, spearman = FALSE) {
  # stop parition if all pairs checked
  if (matrix_is_exhausted(.partition)) {
    .partition$metric <- 0
    .partition$all_done <- TRUE
    return(.partition)
  }

  # find minimum distance
  distance_matrix <- fit_distance_matrix(.partition, spearman = spearman)
  .partition$target <- find_min_distance_variables(distance_matrix)

  # don't check this pair again
  distance_matrix[.partition$target[1], .partition$target[2]] <- NA

  .partition$last_target <- list(
    target = .partition$target,
    distance_matrix = distance_matrix
  )

  .partition
}

fit_distance_matrix <- function(.partition, spearman = FALSE) {
  if (is_not_empty_or_na(.partition$last_target)) {
    return(update_dist(.partition, spearman = spearman))
  }

  distance_matrix <- 1 - corr(.partition$.df, spearman = spearman)
  lower_triangle <- lower.tri(distance_matrix, diag = TRUE)
  distance_matrix[lower_triangle] <- NA

  distance_matrix
}

find_min_distance_variables <- function(.x) {
  indices <- arrayInd(which.min(.x), dim(.x))

  #  get variable names with minimum distance
  c(
    colnames(.x)[indices[1]],
    rownames(.x)[indices[2]]
  )
}

# only find the distances for the new variable
update_dist <- function(.partition, spearman = FALSE) {
  target <- .partition$last_target$target
  distance_matrix <- .partition$last_target$distance_matrix

  column_names <- colnames(distance_matrix)
  variable_names <- names(.partition$reduced_data)
  if (identical(column_names, variable_names)) {
    return(distance_matrix)
  }

  # just refit new variable
  x <- variable_names[length(variable_names)]
  reduced_variable <- .partition$reduced_data %>%
    # should just be last variable added
    # dplyr::select(dplyr::starts_with(!!x)) %>%
    dplyr::pull()

  subset_data <- .partition$reduced_data %>%
    dplyr::select(-dplyr::last_col())

  updated_distances <- purrr::map_dbl(
    subset_data,
    ~1 - corr(.x, reduced_variable, spearman = spearman)
  )

  # add NA to end for diag
  #updated_distances <- c(updated_distances, NA_real_)

  column_names <- colnames(distance_matrix)
  indices <- which(column_names %in% target)
  subset_dist_matrix <- distance_matrix[-indices, -indices]

  updated_dist_matrix <- cbind(subset_dist_matrix, updated_distances)
  updated_dist_matrix <- rbind(updated_dist_matrix, NA_real_)

  #  add new variable name to matrix columns and rows
  new_names <- c(column_names[-indices], x)
  colnames(updated_dist_matrix) <- new_names
  rownames(updated_dist_matrix) <- new_names

  updated_dist_matrix
}

matrix_is_exhausted <- function(.partition) {
  last_target_exists <- is_not_empty_or_na(.partition$last_target)
  if (!last_target_exists) return(last_target_exists)

  # is matrix all NA?
  all(is.na(.partition$last_target$distance_matrix))
}

direct_n_cluster <- function() {

}

