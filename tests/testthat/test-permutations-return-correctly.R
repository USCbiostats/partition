set.seed(1234)

test_that("permuted dfs are correct", {
  pdf <- permute_df(df)
  expect_s3_class(pdf, "data.frame")
  expect_length(pdf, length(df))
  expect_equal(nrow(pdf), nrow(df))
  expect_equal(names(pdf), names(df))
})

mapped_part <- map_partition(df)

test_that("mapped partitions are correct", {
  expect_s3_class(mapped_part, "tbl")
  expect_length(mapped_part, 5)
  nms <- c("target_info", "observed_info", "nclusters", "nreduced", "partition")
  expect_equal(names(mapped_part), nms)
  expect_true(all(purrr::map_lgl(mapped_part$partition, is_partition)))
})

test_that("permutations are correct", {
  perms <- test_permutation(df, nperm = 2)
  expect_s3_class(perms, "tbl")
  expect_length(perms, 9)
  expect_equal(nrow(perms), 6)
  expect_equal(names(perms)[1:5], names(mapped_part))
})
