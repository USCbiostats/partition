plot_information <- function() {

}

#' Plot permutation tests
#'
#' `plot_permutation()` takes the results of [test_permutation()] and plots the
#' distribution of permutted partitions compared to the observed partition.
#'
#' @param permutations a `tibble`, the result of [test_permutation()]
#' @param .plot the variable to plot: observed information, the number of
#'   clusters created, or the number of observed variables reduced
#' @param perm_color the color of the permutation fill
#' @param obs_color the color of the observed statistic line
#' @param labeller the facet label
#' @param geom the `geom` to use. The default is `geom_density`.
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' set.seed(123)
#' df <- simulate_block_data(c(3, 4, 5), lower_corr = .4, upper_corr = .6, n = 100)
#'
#' test_permutation(df, partitioner = part_pc1(), nperm = 10) %>%
#'   plot_permutation("nclusters")
#'
#' @importFrom rlang !!
#' @importFrom ggplot2 geom_density
plot_permutation <- function(permutations,
                             .plot = c("information", "nclusters", "nreduced"),
                             labeller = "target information:",
                             perm_color = "#56B4EA",
                             obs_color = "#CC78A8",
                             geom = geom_density) {
  .plot <- match.arg(.plot)
  .plot <- ifelse(.plot == "information", "observed_info", .plot)
  xlabel <- dplyr::case_when(
    #  TODO: not sure about this labels
    .plot == "observed_info" ~ "observed information",
    .plot == "nclusters" ~ "n clusters created",
    .plot == "nreduced" ~ "n observed variables reduced to clusters"
  )

  plot_sym <- rlang::sym(.plot)

  label_info <- function(target) paste(labeller, target)

  p <- permutations %>%
    dplyr::select(target_info, permutation) %>%
    tidyr::unnest() %>%
    ggplot2::ggplot(ggplot2::aes(x = !!plot_sym)) +
    geom(
      ggplot2::aes(fill = "permuted"),
      color = NA
    ) +
    ggplot2::geom_vline(
      data = permutations,
      ggplot2::aes(xintercept = !!plot_sym, col = "observed"),
      size = 1.2
    ) +
    ggplot2::facet_wrap(
      target_info ~ .,
      ncol = 1,
      scales = "free_y",
      labeller = ggplot2::as_labeller(label_info)
    ) +
    ggplot2::xlab(xlabel) +
    ggplot2::scale_color_manual(name = NULL, values = obs_color) +
    ggplot2::scale_fill_manual(name = NULL, values = perm_color) +
    ggplot2::theme(legend.spacing.y = ggplot2::unit(0, "mm")) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 99))

  if (.plot == "information") {
    p <- p + ggplot2::xlim(0, 1)
  } else {
    p <- p + ggplot2::xlim(0, NA)
  }

  p
}
