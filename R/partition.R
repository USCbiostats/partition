as_partition <- function(reducer, director, metric) {
  function(.df, threshold) {
    .df %>%
      director() %>%
      reducer() %>%
      metric()
  }
}

as_partition_step <- function(.df, reduced_data = NA, target = NA, metric_value = NA, ...) {
  structure(
      list(
        .df = .df,
        target = target,
        last_target = NA,
        reduced_data = reduced_data,
        metric_value = metric_value,
        ...
      ),
      class = "partition_step"
    )
}
