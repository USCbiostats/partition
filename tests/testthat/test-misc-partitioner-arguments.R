set.seed(1234)

expect_identical_partition <- function(.x, .y, same_partitioner = TRUE) {
  expect_identical(.x$reduced_data, .y$reduced_data)
  expect_identical(.x$mapping_key, .y$mapping_key)
  expect_identical(.x$threshold, .y$threshold)
  if (same_partitioner) {
    expect_equal(.x$partitioner, .y$partitioner, ignore_function_env = TRUE)
  } else {
    expect_failure(expect_identical(.x$partitioner, .y$partitioner))
  }
}

test_that("accelerated functions return correctly", {
  expect_equal(icc(df8), icc(df8, method = "c"))
  expect_equal(scaled_mean(df8), scaled_mean(df8, method = "c"))
  expect_equal(round(icc(df8), 3), 0.608, tolerance = .001)
  expect_equal(round(partition:::minR2_c(as.matrix(df8))$minr2, 3), 0.588, tolerance = .001)
  expect_equal(round(mutual_information(df8)$standardized_mi, 3), 0.305, tolerance = .001)
  expect_equal(round(partition:::pca_c(as.matrix(df8))$pct_var, 3), 0.658, tolerance = .001)
})

test_that("spearman distance works", {
  expect_s3_class(partition(df8, .65, partitioner = part_icc(spearman = TRUE)), "partition")
})

test_that("linear and binary searches find the same partition", {
  lin <- partition(df8, .65, partitioner = part_kmeans(search = "linear"))
  bin <- partition(df8, .65, partitioner = part_kmeans(search = "binary"))
  expect_identical_partition(lin, bin, same_partitioner = FALSE)

  search_forward <- partition(df8, .65, partitioner = part_kmeans(search = "linear", init_k = 2))
  search_back <- partition(df8, .65, partitioner = part_kmeans(search = "linear", init_k = 7))
  expect_identical_partition(search_forward, bin, same_partitioner = FALSE)
  expect_identical_partition(search_back, bin, same_partitioner = FALSE)
})

test_that("init k searches find the same partition", {
  search_k <- partition(df8, .65, partitioner = part_kmeans())
  exact_k <- partition(df8, .65, partitioner = part_kmeans(init_k = 6))
  above_k <- partition(df8, .65, partitioner = part_kmeans(init_k = 7))
  expect_identical_partition(search_k, exact_k)
  expect_identical_partition(search_k, above_k)

  search_back <- partition(df8, .0001, partitioner = part_kmeans(search = "linear", init_k = 3))
  expect_s3_class(search_back, "partition")
})

test_that("r kmeans algorithms work", {
  k_hw <- partition(df8, .65, partitioner = part_kmeans(algorithm = "Hartigan-Wong"))
  k_l <- partition(df8, .65, partitioner = part_kmeans(algorithm = "Lloyd"))
  k_f <- partition(df8, .65, partitioner = part_kmeans(algorithm = "Forgy"))
  k_m <- partition(df8, .65, partitioner = part_kmeans(algorithm = "MacQueen"))

  expect_s3_class(k_hw, "partition")
  expect_s3_class(k_l, "partition")
  expect_s3_class(k_f, "partition")
  expect_s3_class(k_m, "partition")
})
