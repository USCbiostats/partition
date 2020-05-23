#' Plot partitions
#'
#'  `plot_stacked_area_clusters()` and `plot_area_clusters()` plot the partition
#'  against a permuted partition. `plot_ncluster()` plots the number of
#'  variables per cluster. If `.partition` is the result of [map_partition()] or
#'  [test_permutation()], `plot_ncluster()` facets the plot by each `partition`.
#'  `plot_information()` plots a histogram or density plot of the information of
#'  each variable in the `partition`. If `.partition` is the result of
#'  [map_partition()] or [test_permutation()], `plot_information()` plots a
#'  scatterplot of the targeted vs. observed information with a 45 degree line
#'  indicating perfect alignment.
#'
#' @param .partition either a `partition` or a `tibble`, the result of
#'   [map_partition()] or [test_permutation()]
#' @inheritParams partition
#' @inheritParams test_permutation
#' @param obs_color the color of the observed partition
#' @param perm_color the color of the permuted partition
#' @param stack_colors the colors of the cluster sizes
#' @param show_n the number of reduced variables to plot
#' @param fill the color of the fill for `geom`
#' @param color the color of the `geom`
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
#' df %>%
#'   partition(.6, partitioner = part_pc1()) %>%
#'   plot_ncluster()
#'
#' @rdname plot_partitions
#' @importFrom graphics hist
plot_area_clusters <- function(.data, partitioner = part_icc(),
                               information = seq(0.1, 0.5, length.out = 25),
                               ..., obs_color = "#E69F00", perm_color = "#56B4E9") {
  plot_df <- map_single_permutation(
    .data,
    partitioner = partitioner,
    ...,
    information = information
  )

  cluster_area_plot(
    plot_df,
    breaks = pretty(information),
    obs_color = obs_color,
    perm_color = perm_color
  )
}

#' @export
#' @rdname plot_partitions
plot_stacked_area_clusters <- function(.data, partitioner = part_icc(),
                               information = seq(0.1, 0.5, length.out = 25),
                               ...,  stack_colors = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")) {
  plot_df <- map_single_permutation(
    .data,
    partitioner = partitioner,
    ...,
    information = information
  )

  cluster_stacked_area_plot(
    plot_df,
    breaks = pretty(information),
    stack_colors = stack_colors
  )
}

map_single_permutation  <- function(.data, partitioner, ..., information) {
  parts <- map_partition(.data, partitioner = partitioner, ..., information = information)
  perm_parts <- map_partition(permute_df(.data), partitioner = partitioner, ..., information = information)
  groups <- c("1", "2-4", "5-8", "9-16", "17-32", ">32")
  breaks <- c(0, 1, 4, 8, 16, 32, 10^10)
  purrr::map2_dfr(
    parts$partition,
    perm_parts$partition,
    map_cluster_counts,
    groups,
    breaks
  )
}

cluster_stacked_area_plot <- function(plot_df, breaks, stack_colors = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")) {
  label_var <- function(x) paste(x, ifelse(x == "1", "variable\n per cluster", "variables\n per cluster"))
  filter_part <- function(part) function(x) dplyr::filter(x, partition == part)
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = target_information,
      y = counts,
      col = groups,
      fill = groups
    )) +
    ggplot2::stat_smooth(
      geom = "area",
      position = "stack",
      size = .2,
      alpha = .7,
      color = NA,
      method = "loess",
      formula = y ~ x
    ) +
    ggplot2::scale_x_continuous(
      limits = c(min(breaks), max(breaks)),
      breaks = breaks
    ) +
    ggplot2::labs(
      color = "n raw features\nper reduced",
      fill = "n raw features\nper reduced",
      x = "minimum information",
      y = "n reduced"
    ) +
    ggplot2::ylim(0, NA) +
    ggplot2::facet_wrap(~partition, ncol = 1) +
    ggplot2::scale_color_manual(values = stack_colors) +
    ggplot2::scale_fill_manual(values = stack_colors)
}

cluster_area_plot <- function(plot_df, breaks, obs_color = "#E69F00", perm_color = "#56B4E9") {
  label_var <- function(x) paste(x, ifelse(x == "1", "variable\n per cluster", "variables\n per cluster"))
  filter_part <- function(part) function(x) dplyr::filter(x, partition == part)
  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = target_information,
      y = counts,
      col = partition,
      fill = partition
    )) +
    ggplot2::stat_smooth(
      data = filter_part("permuted"),
      geom = "area",
      size = .2,
      alpha = .7,
      color = NA,
      method = "loess",
      formula = y ~ x
    ) +
    ggplot2::stat_smooth(
      data = filter_part("observed"),
      geom = "area",
      size = .2,
      alpha = .7,
      color = NA,
      method = "loess",
      formula = y ~ x
    ) +
    ggplot2::geom_smooth(
      data = filter_part("permuted"),
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      size = .2,
      na.rm = TRUE
    ) +
    ggplot2::geom_smooth(
      data = filter_part("observed"),
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      size = .2,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(
      limits = c(min(breaks), max(breaks)),
      breaks = breaks
    ) +
    ggplot2::labs(
      color = NA,
      fill = NA,
      x = "minimum information",
      y = "n reduced"
    ) +
    ggplot2::ylim(0, NA) +
    ggplot2::facet_wrap(~groups, labeller = ggplot2::as_labeller(label_var)) +
    ggplot2::scale_color_manual(values = c(obs_color, perm_color)) +
    ggplot2::scale_fill_manual(values = c(obs_color, perm_color))
}

map_cluster_counts <- function(part, perm_part, groups, breaks) {
  n_groups <- length(groups)
  groups <- factor(groups, levels = groups)
  counts <- get_counts(part, breaks)
  perm_counts <- get_counts(perm_part, breaks)
  info <- get_mean_info(part)
  perm_info <- get_mean_info(perm_part)
  tibble::tibble(
    partition = c(rep("observed", n_groups), rep("permuted", n_groups)),
    groups = rep(groups, 2),
    target_information = c(rep(part$threshold, n_groups), rep(perm_part$threshold, n_groups)),
    obs_information = c(rep(info, n_groups), rep(perm_info, n_groups)),
    counts = c(counts, perm_counts)
  )
}

get_counts <- function(.partition, breaks) {
  mappings <- .partition %>%
    unnest_mappings()

  histogram <- mappings[["variable"]] %>%
    table() %>%
    hist(breaks = breaks, plot = FALSE)

  histogram[["counts"]]
}

get_mean_info <- function(.partition) {
  mappings <- .partition %>%
    mapping_key()

  mean(mappings[["information"]], na.rm = TRUE)
}

#' @export
#' @rdname plot_partitions
plot_ncluster <- function(.partition, show_n = 100, fill = "#0172B1", color = NA, labeller = "target information:") {
  if (is_partition(.partition)) return(plot_clusters(.partition, show_n, fill, color))

  label_info <- function(target) paste(labeller, target)

  .partition %>%
    dplyr::mutate(mapping_key = purrr::map(partition, mapping_key)) %>%
    dplyr::select(target_info, mapping_key) %>%
    tidyr::unnest(cols = c(mapping_key)) %>%
    plot_clusters(show_n, fill, color) +
      ggplot2::facet_wrap(
        ~target_info,
        ncol = 1,
        labeller = ggplot2::as_labeller(label_info),
        scales = "free_y"
      )
}

#' @export
#' @rdname plot_partitions
plot_information <- function(.partition, fill = "#0172B1", color = NA,
                             geom = ggplot2::geom_density) {
  if (is_partition(.partition)) return(
    plot_info_hist(
      .partition,
      fill = fill,
      color = color,
      geom = geom
    )
  )

 .partition %>%
    ggplot2::ggplot(ggplot2::aes(x = observed_info, target_info)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, col = "grey80", size = .7) +
    ggplot2::geom_point() +
    ggplot2::xlim(0, 1.05) +
    ggplot2::ylim(0, 1.05) +
    ggplot2::xlab("observed information") +
    ggplot2::ylab("targeted information")
}

plot_clusters <- function(.partition, show_n = 100, fill = "#0172B1", color = NA) {
  if (is_partition(.partition)) .partition <- mapping_key(.partition)

  .partition %>%
    dplyr::mutate(
      n = purrr::map_int(mapping, length),
      variable = forcats::fct_reorder(variable, n)
    ) %>%
    dplyr::arrange(dplyr::desc(n), dplyr::desc(variable)) %>%
    dplyr::slice(1:show_n) %>%
    ggplot2::ggplot(ggplot2::aes(x = variable, y = n)) +
    ggplot2::geom_col(fill = fill, color = color) +
    ggplot2::ylab("n raw features") +
    ggplot2::coord_flip()
}

plot_info_hist <- function(.partition, fill = "#0172B1", color = NA,
                             geom = ggplot2::geom_density) {
   .partition %>%
    mapping_key() %>%
    ggplot2::ggplot(ggplot2::aes(x = information)) +
    geom(fill = fill, color = color) +
    ggplot2::xlim(0, 1.05) +
    ggplot2::xlab("observed information")
}

#' Plot permutation tests
#'
#' `plot_permutation()` takes the results of [test_permutation()] and plots the
#' distribution of permuted partitions compared to the observed partition.
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
#' @importFrom rlang !!
#' @importFrom ggplot2 geom_density
plot_permutation <- function(permutations,
                             .plot = c("information", "nclusters", "nreduced"),
                             labeller = "target information:",
                             perm_color = "#56B4EA",
                             obs_color = "#CC78A8",
                             geom = ggplot2::geom_density) {
  .plot <- match.arg(.plot)
  .plot <- ifelse(.plot == "information", "observed_info", .plot)
  xlabel <- dplyr::case_when(
     # TODO: not sure about this labels
    .plot == "observed_info" ~ "observed information",
    .plot == "nclusters" ~ "n clusters created",
    .plot == "nreduced" ~ "n observed variables reduced to clusters"
  )

  plot_sym <- rlang::sym(.plot)

  label_info <- function(target) paste(labeller, target)

  p <- permutations %>%
    dplyr::select(permutation) %>%
    tidyr::unnest(cols = c(permutation)) %>%
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

  if (.plot == "observed_info") {
    p <- p + ggplot2::xlim(0, 1)
  } else {
    p <- p + ggplot2::xlim(0, NA)
  }

  p
}
