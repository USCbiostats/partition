mutual_information <- function(.data) {
  discretized_data <- infotheo::discretize(.data, disc = "equalfreq")
  row_means <- rowMeans(.data, na.rm = TRUE)
  discretized_means <- infotheo::discretize(row_means, disc = "equalfreq")
  mi <- infotheo::mutinformation(discretized_data, discretized_means, method = "shrink")
  entropy <- dplyr::bind_cols(discretized_data, discretized_means) %>%
    infotheo::entropy(method = "shrink")
  # standardize MI by entropy
  standardized_mi <- mi / entropy
  scaled_row_means <- as.numeric(scale(row_means))
  list(standardized_mi = standardized_mi, scaled_row_means = scaled_row_means)
}
