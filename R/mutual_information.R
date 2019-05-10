#' Calculate the standardized mutual information of a data set
#'
#' `mutual_information` calculate the standardized mutual information of a data
#' set using the `infotheo` package.
#'
#' @param .data a dataframe of numeric values
#'
#' @return a list containing the standardized MI and the scaled row means
#' @export
#'
#' @examples
#' library(dplyr)
#' iris %>%
#'   select_if(is.numeric) %>%
#'   mutual_information()
#'
mutual_information <- function(.data) {
  #  discretize data
  discretized_data <- infotheo::discretize(.data, disc = "equalfreq")
  row_means <- rowMeans(.data, na.rm = TRUE)
  row_means <- swap_nans(row_means)
  discretized_means <- infotheo::discretize(row_means, disc = "equalfreq")

  #  calculate MI and entropy
  mi <- infotheo::mutinformation(discretized_data, discretized_means, method = "shrink")
  entropy <- dplyr::bind_cols(discretized_data, discretized_means) %>%
    infotheo::entropy(method = "shrink")

  #  standardize MI by entropy, row means by SD
  standardized_mi <- mi / entropy
  scaled_row_means <- as.numeric(scale(row_means))

  list(standardized_mi = standardized_mi, scaled_row_means = scaled_row_means)
}
