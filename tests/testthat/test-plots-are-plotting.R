set.seed(1234)
perms <- test_permutation(df, nperm = 2)

test_that("plots are plotting", {
  g1 <- plot_permutation(perms)
  g2 <- plot_permutation(perms, .plot = "nclusters")
  g3 <- plot_permutation(perms, .plot = "nreduced")

  expect_s3_class(ggplot2::ggplot_build(g1), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(g2), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(g3), "ggplot_built")

  g4 <- plot_information(perms$partition[[6]])
  g5 <- plot_ncluster(perms$partition[[6]])

  expect_s3_class(ggplot2::ggplot_build(g4), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(g5), "ggplot_built")

  g6 <- plot_information(perms)
  g7 <- plot_ncluster(perms)

  expect_s3_class(ggplot2::ggplot_build(g6), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(g7), "ggplot_built")

  g8 <- plot_area_clusters(df, information = seq(0.1, 0.5, length.out = 10))
  expect_s3_class(ggplot2::ggplot_build(g8), "ggplot_built")

  g9 <- plot_stacked_area_clusters(df, information = seq(0.1, 0.5, length.out = 10))
  expect_s3_class(ggplot2::ggplot_build(g8), "ggplot_built")
})
