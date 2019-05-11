context("test-misc-partitioner-arguments")
skip_on_os("windows")

set.seed(1234)
df <- simulate_block_data(8, lower_corr = .6, upper_corr = .65, n = 100)

expect_identical_partition <- function(.x, .y, same_partitioner = TRUE) {
  expect_identical(.x$reduced_data, .y$reduced_data)
  expect_identical(.x$mapping_key, .y$mapping_key)
  expect_identical(.x$threshold, .y$threshold)
  if (same_partitioner) {
    expect_equal(.x$partitioner, .y$partitioner)
  } else {
    expect_failure(expect_identical(.x$partitioner, .y$partitioner))
  }
}

test_that("accelerated functions return correctly", {
  expect_equal(icc(df), icc(df, method = "c"))
  expect_equal(scaled_mean(df), scaled_mean(df, method = "c"))
  expect_equal(icc(df), 0.6081562, tolerance = 1e-5)
  expect_equal(partition2:::minR2_c(as.matrix(df))$minr2, 0.5884837, tolerance = 1e-5)
  expect_equal(mutual_information(df)$standardized_mi, 0.3051297, tolerance = 1e-5)
  expect_equal(partition2:::pca_c(as.matrix(df))$pct_var, 0.6582012, tolerance = 1e-5)
})

test_that("spearman distance works", {
  expect_is(partition(df, .65, partitioner = part_icc(spearman = TRUE)), "partition")
})

test_that("linear and binary searches find the same partition", {
  lin <- partition(df, .65, partitioner = part_kmeans(search = "linear"))
  bin <- partition(df, .65, partitioner = part_kmeans(search = "binary"))
  expect_identical_partition(lin, bin, same_partitioner = FALSE)

  search_forward <- partition(df, .65, partitioner = part_kmeans(search = "linear", init_k = 2))
  search_back <- partition(df, .65, partitioner = part_kmeans(search = "linear", init_k = 7))
  expect_identical_partition(search_forward, bin, same_partitioner = FALSE)
  expect_identical_partition(search_back, bin, same_partitioner = FALSE)
})

test_that("init k searches find the same partition", {
  search_k <- partition(df, .65, partitioner = part_kmeans())
  exact_k <- partition(df, .65, partitioner = part_kmeans(init_k = 6))
  above_k <- partition(df, .65, partitioner = part_kmeans(init_k = 7))
  expect_identical_partition(search_k, exact_k)
  expect_identical_partition(search_k, above_k)

  search_back <- partition(df, .0001, partitioner = part_kmeans(search = "linear", init_k = 3))
  expect_is(search_back, "partition")
})

test_that("r kmeans algorithms work", {
  k_hw <- partition(df, .65, partitioner = part_kmeans(algorithm = "Hartigan-Wong"))
  k_l <- partition(df, .65, partitioner = part_kmeans(algorithm = "Lloyd"))
  k_f <- partition(df, .65, partitioner = part_kmeans(algorithm = "Forgy"))
  k_m <- partition(df, .65, partitioner = part_kmeans(algorithm = "MacQueen"))

  expect_is(k_hw, "partition")
  expect_is(k_l, "partition")
  expect_is(k_f, "partition")
  expect_is(k_m, "partition")
})
