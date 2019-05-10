#' Create a custom director
#'
#' @template describe_director
#'
#' @param .pairs a function that returns a matrix of targets (e.g. a distance matrix of variables)
#' @param .target a function that returns a vector of targets (e.g. the minimum pair)
#' @param ... Extra arguments passed to `.f`.
#'
#' @return a function to use in [`as_partitioner()`]
#' @export
#'
#' @examples
#' # use euclidean distance to calculate distances
#' euc_dist <- function(.data) as.matrix(dist(t(.data)))
#'
#' # find the pair with the minimum distance
#' min_dist <- function(.x) {
#'   indices <- arrayInd(which.min(.x), dim(as.matrix(.x)))
#'
#'   #  get variable names with minimum distance
#'   c(
#'     colnames(.x)[indices[1]],
#'     colnames(.x)[indices[2]]
#'   )
#' }
#'
#' as_director(euc_dist, min_dist)
#'
as_director <- function(.pairs, .target, ...) {
  function(.partition_step, ...) {
    # stop partition if all variables reduced
    if (ncol(.partition_step$reduced_data) == 1) {
      .partition_step$metric <- 0
      return(all_done(.partition_step))
    }
    pairwise <- .pairs(.partition_step$reduced_data, ...)
    pairwise <- tag_previous_targets(pairwise, .partition_step$target_history)
    # stop partition if all pairs checked
    if (all(is.na(pairwise))) {
      .partition_step$metric <- 0
      return(all_done(.partition_step))
    }
    .partition_step$target <- .target(pairwise)
    .partition_step$target_history <- add_history(.partition_step)
    .partition_step$last_target <- list(target = .partition_step$target)

    .partition_step
  }
}

add_history <- function(.partition_step) {
  if (is.null(.partition_step$target_history)) {
    return(
      data.frame(
        first = .partition_step$target[1],
        second = .partition_step$target[2],
        stringsAsFactors = FALSE
      )
    )
  }

  rbind(
    .partition_step$target_history,
    data.frame(
      first = .partition_step$target[1],
      second = .partition_step$target[2],
      stringsAsFactors = FALSE
    )
  )
}

tag_previous_targets <- function(.matrix, .history) {
  .matrix[upper.tri(.matrix, diag = TRUE)] <- NA
  if (!is.null(.history)) {
    .history <- .history %>%
      dplyr::filter(
        first %in% rownames(.matrix),
        second %in% colnames(.matrix)
      )

    if (nrow(.history) == 0) return(.matrix)
    .matrix[.history$first, .history$second] <- NA
  }
  .matrix
}

#' Target based on minimum distance matrix
#'
#' @template describe_director
#'
#' @description `direct_distance()` fits a distance matrix using either Pearson's or
#' Spearman's correlation and finds the pair with the smallest distance to
#' target. If the distance matrix already exists, `direct_distance()` only
#' fits the distances for any new reduced variables.
#' `direct_distance_pearson()` and `direct_distance_spearman()` are
#' convenience functions that directly call the type of distance matrix.
#'
#' @template partition_step
#' @param spearman Logical. Use Spearman's correlation?
#'
#' @export
#'
#' @rdname direct_distance
direct_distance <- function(.partition_step, spearman = FALSE) {
  # stop partition if all pairs checked
  if (matrix_is_exhausted(.partition_step)) {
    .partition_step$metric <- 0
    return(all_done(.partition_step))
  }

  # find minimum distance
  distance_matrix <- fit_distance_matrix(.partition_step, spearman = spearman)
  .partition_step$target <- find_min_distance_variables(distance_matrix)

  # don't check this pair again
  distance_matrix[.partition_step$target[1], .partition_step$target[2]] <- NA

  # store target and distance matrix to use later
  .partition_step$last_target <- list(
    target = .partition_step$target,
    distance_matrix = distance_matrix
  )

  .partition_step
}

#' @export
#' @rdname direct_distance
direct_distance_pearson <- function(.partition_step) {
  direct_distance(.partition_step, spearman = FALSE)
}

#' @export
#' @rdname direct_distance
direct_distance_spearman <- function(.partition_step) {
  direct_distance(.partition_step, spearman = TRUE)
}

#' Target based on K-means clustering
#'
#' @template describe_director
#'
#' @description `direct_k_cluster()` assigns each variable to a cluster using
#'  K-means. As the partition looks for the best reduction,
#'  `direct_k_cluster()` iterates through values of `k` to assign clusters.
#'  This search is handled by the binary search method by default and thus
#'  does not necessarily need to fit every value of k.
#'
#' @template partition_step
#' @param search The search method. Binary search is generally more efficient
#'   but linear search can be faster in very low dimensions.
#' @param algorithm The K-Means algorithm to use. The default is a fast version
#'   of the LLoyd algorithm written in armadillo. The rest are options in
#'   [kmeans()]. In general, armadillo is fastest, but the other algorithms can
#'   be faster in high dimensions.
#' @param init_k The initial k to test. If `NULL`, then the initial k is the
#'   threshold times the number of variables.
#' @param seed The seed to set for reproducibility
#'
#' @export
direct_k_cluster <- function(.partition_step,
                             algorithm = c("armadillo", "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
                             search = c("binary", "linear"),
                             init_k = NULL,
                             seed = 1L) {
  search_method <- match.arg(search)
  algorithm <- match.arg(algorithm)

  #  use initial k argument if specified
  if (is.null(.partition_step$k) & !is.null(init_k)) .partition_step$k <- init_k
  #  take an initial guess for `k` if not specified
  if (is.null(.partition_step$k)) .partition_step$k <- guess_init_k(.partition_step)

  #  stop partition if all k checked
  if (k_exhausted(.partition_step)) {
    .partition_step$metric <- 0
    return(all_done(.partition_step))
  }

  #  pick kmeans algorithm
  kmean_f <- find_algorithm(algorithm, seed = seed)

  #  assign each variable to a cluster
  .partition_step$target <- kmean_f(as.matrix(.partition_step$.df), .partition_step$k)

  #  the binary search method requires assigning k-1 clusters to check boundary
  #  unless k = 1 (so don't check k = 0)
  if (search_method == "binary" && .partition_step$k != 1) {
    .partition_step$target_k1 <- kmean_f(
      as.matrix(.partition_step$.df),
      .partition_step$k - 1
    )
  }

  .partition_step
}


#' Efficiently fit correlation coefficient for matrix or two vectors
#'
#' @param x a matrix or vector
#' @param y a vector. Optional.
#' @param spearman Logical. Use Spearman's correlation?
#'
#' @return a numeric vector, the correlation coefficient
#' @export
#'
#' @examples
#' library(dplyr)
#' # fit for entire data set
#' iris %>%
#'   select_if(is.numeric) %>%
#'   corr()
#'
#' # just fit for two vectors
#' corr(iris$Sepal.Length, iris$Sepal.Width)
#'
#' @importFrom stats complete.cases na.omit
  corr <- function(x, y = NULL, spearman = FALSE) {
    if (is.null(y)) {
      dim_names <- names(x)
      if (spearman) {
        #  use ranks
        x <- apply_rank(as.matrix(na.omit(x)))
      }

      #  correlation for matrices
      correlation <- corr_c_mat(na.omit(as.matrix(x)))

      attr(correlation, "dimnames") <- list(dim_names, dim_names)
      return(correlation)
    }
    #  use pairwise complete cases if any missing
    complete_cases <- complete.cases(x, y)
    x <- x[complete_cases]
    y <- y[complete_cases]

    if (spearman) {
      #  use ranks
      x <- rank_c(x)
      y <- rank_c(y)
    }

    #  correlation for two vectors
    corr_c_2vec(x, y)
  }

#' Fit a distance matrix using correlation coefficients
#'
#' @template partition_step_param
#' @param spearman Logical. Use Spearman's correlation?
#'
#' @return a `matrix` of size `p` by `p`
#' @keywords internal
fit_distance_matrix <- function(.partition_step, spearman = FALSE) {
  #  if the distance matrix exists, just update it
  if (is_not_empty_or_na(.partition_step$last_target)) {
    return(update_dist(.partition_step, spearman = spearman))
  }

  #  fit the distance matrix using 1 - correlation
  distance_matrix <- 1 - corr(.partition_step$.df, spearman = spearman)
  lower_triangle <- lower.tri(distance_matrix, diag = TRUE)
  distance_matrix[lower_triangle] <- NA

  distance_matrix
}

#' Find the index of the pair with the smallest distance
#'
#' @param .x a distance matrix
#'
#' @return a character vector with the names of the minimum pair
#' @keywords internal
find_min_distance_variables <- function(.x) {
  indices <- arrayInd(which.min(.x), dim(.x))

  #  get variable names with minimum distance
  c(
    colnames(.x)[indices[1]],
    rownames(.x)[indices[2]]
  )
}

#' Only fit the distances for a new variable
#'
#' @template partition_step_param
#' @param spearman Logical. Use Spearman's correlation?
#'
#' @return a `matrix`
#' @keywords internal
update_dist <- function(.partition_step, spearman = FALSE) {
  target <- .partition_step$last_target$target
  distance_matrix <- .partition_step$last_target$distance_matrix

  column_names <- colnames(distance_matrix)
  variable_names <- names(.partition_step$reduced_data)
  if (identical(column_names, variable_names)) {
    return(distance_matrix)
  }

  # just refit new variable
  x <- variable_names[length(variable_names)]
  reduced_variable <- .partition_step$reduced_data[[ncol(.partition_step$reduced_data)]]

  subset_data <- .partition_step$reduced_data[, -ncol(.partition_step$reduced_data), drop = FALSE]

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

#' Have all pairs of variables been checked for metric?
#'
#' @template partition_step_param
#'
#' @return logical: `TRUE` or `FALSE`
#' @keywords internal
matrix_is_exhausted <- function(.partition_step) {
  last_target_exists <- is_not_empty_or_na(.partition_step$last_target)
  if (!last_target_exists) return(last_target_exists)

  # is matrix all NA?
  all(is.na(.partition_step$last_target$distance_matrix))
}

#' Guess initial `k` based on threshold and `p`
#'
#' @template partition_step_param
#'
#' @return an integer
#' @keywords internal
guess_init_k <- function(.partition_step) {
  k <- round(.partition_step$threshold * ncol(.partition_step$.df))
  ifelse(k < 1, 1, k)
}

#' Have all values of `k` been checked for metric?
#'
#' @template partition_step_param
#'
#' @return logical: `TRUE` or `FALSE`
#' @keywords internal
k_exhausted <- function(.partition_step) {
  k_0 <- .partition_step$k == 0
  k_max <- .partition_step$k == ncol(.partition_step$.df)

  k_0 || k_max
}

#' Which kmeans algorithm to use?
#'
#' `find_algorithm()` returns a function to assign k-means cluster.
#' `kmean_assignment_r()` wraps around [kmeans()] to pull the correct
#' assignments.
#'
#' @param algorithm the kmeans algorithm to use
#'
#' @return a kmeans function
#' @keywords internal
#' @rdname kmeans_helpers
find_algorithm <- function(algorithm, seed) {
  switch(
    algorithm,
    "armadillo" = purrr::partial(kmean_assignment_c, seed = seed),
    "Hartigan-Wong" = purrr::partial(kmean_assignment_r, seed = seed),
    "Lloyd" = purrr::partial(kmean_assignment_r, algorithm = "Lloyd", seed = seed),
    "Forgy" = purrr::partial(kmean_assignment_r, algorithm = "Forgy", seed = seed),
    "MacQueen" = purrr::partial(kmean_assignment_r, algorithm = "MacQueen", seed = seed),
  )
}

#' @rdname kmeans_helpers
#' @importFrom stats kmeans
kmean_assignment_c <- function(.data, k, n_iter = 10L, verbose = FALSE, seed = 1L) {
  kmean_assignment(na.omit(.data), k = k, n_iter = n_iter, verbose = verbose, seed = seed)
}

#' @rdname kmeans_helpers
#' @importFrom stats kmeans
kmean_assignment_r <- function(.data, k, algorithm = "Hartigan-Wong", seed = 1L) {
  set.seed(seed)
  kmeans(t(na.omit(.data)), centers = k, algorithm = algorithm, nstart = 25)[["cluster"]]
}
