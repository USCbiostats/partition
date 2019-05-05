context("test-plots-are-plotting")
set.seed(1234)
df <- simulate_block_data(5, lower_corr = .5, upper_corr = .65, n = 100)
perms <- test_permutation(df, nperm = 2)

test_that("plots are plotting", {
  g1 <- plot_permutation(perms)
  g2 <- plot_permutation(perms, .plot = "nclusters")
  g3 <- plot_permutation(perms, .plot = "nreduced")

  expect_is(ggplot2::ggplot_build(g1), "ggplot_built")
  expect_is(ggplot2::ggplot_build(g2), "ggplot_built")
  expect_is(ggplot2::ggplot_build(g3), "ggplot_built")

  g4 <- plot_information(perms$partition[[6]])
  g5 <- plot_ncluster(perms$partition[[6]])

  expect_is(ggplot2::ggplot_build(g4), "ggplot_built")
  expect_is(ggplot2::ggplot_build(g5), "ggplot_built")

  g6 <- plot_information(perms)
  g7 <- plot_ncluster(perms)

  expect_is(ggplot2::ggplot_build(g6), "ggplot_built")
  expect_is(ggplot2::ggplot_build(g7), "ggplot_built")
})