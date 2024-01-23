set.seed(123)

prt <- super_partition(df8, threshold = .6, cluster_size = 3, verbose = FALSE, progress_bar = FALSE)
reduced_data <- prt[["reduced_data"]]
map_key <- prt[["mapping_key"]]
threshold <- prt[["threshold"]]
prtnr <- prt[["partitioner"]]

ind_prt <- super_partition(ind_df, threshold = .6, cluster_size = 3, verbose = FALSE, progress_bar = FALSE)

test_that("partition object is returning correctly", {
  # partition object
  expect_s3_class(prt, "partition")
  expect_named(prt)
  expect_equal(names(prt), c("reduced_data", "mapping_key", "threshold", "partitioner"))

  # reduced data
  expect_s3_class(reduced_data, c("tbl_df", "tbl", "data.frame"))
  expect_named(reduced_data)
  expect_equal(names(reduced_data), c("block1_x3", "reduced_var_1", "reduced_var_2", "reduced_var_3"))
  all_numeric <- all(purrr::map_chr(reduced_data, class) == "numeric")
  expect_true(all_numeric)

  # mapping key
  expect_s3_class(map_key, c("tbl_df", "tbl", "data.frame"))
  expect_named(map_key)
  expect_equal(names(map_key), c("variable", "mapping", "information", "indices", "super_partition"))
  expect_type(map_key[["variable"]], "character")
  expect_type(map_key[["mapping"]], "list")
  expect_type(map_key[["information"]], "double")
  expect_type(map_key[["indices"]], "list")
  expect_type(map_key[["super_partition"]], "double")
  expect_true(all_numeric)

  # threshold
  expect_equal(threshold, .6)
  expect_type(threshold, "double")

  # partitioner
  expect_s3_class(prtnr, "partitioner")

  # print
  expect_output(print(partition(df, .1)))
})


test_that("dimensions are consistent", {
  expect_length(prt, 4)
  expect_equal(nrow(reduced_data), 100)

  # reduced data
  expect_length(reduced_data, 4)
  expect_equal(nrow(reduced_data), 100)
  expect_lte(length(reduced_data), length(df))
  expect_equal(nrow(reduced_data), nrow(df))

  # mapping key
  reduced_map <- filter_reduced(prt)
  expect_length(map_key, 5)
  expect_equal(nrow(map_key), 4)
  expect_equal(nrow(reduced_map), 3)
  map_lengths <- purrr::map_int(map_key[["mapping"]], length)
  expect_true(all(map_lengths == c(1, 2, 2, 3)))
  index_lengths <- purrr::map_int(map_key[["indices"]], length)
  expect_true(all(index_lengths == map_lengths))

  # partitioner
  expect_length(prtnr, 3)
})

test_that("independent data set doesn't reduce", {
  expect_identical(ind_prt[["reduced_data"]], ind_df)
  expect_equal(nrow(filter_reduced(ind_prt)), 0)
  expect_equal(nrow(ind_prt[["mapping_key"]]), ncol(ind_df))
})

test_that("errors work as expected", {
  expect_error(super_partition(df8, threshold = -2), "Threshold must be between 0 and 1.")
  expect_error(super_partition(df8, threshold = 2), "Threshold must be between 0 and 1.")
  expect_error(super_partition(df8, threshold = 0.6, x = "block"), "The prefix for new variable names, block, is contained within existing data column names. Please choose a different prefix to avoid errors.")
})
