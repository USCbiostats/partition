context("test-partitioners-reduce-correctly")
skip_on_appveyor()

set.seed(1234)
df <- simulate_block_data(5, lower_corr = .5, upper_corr = .65, n = 100)
ind_df <- purrr::map_dfc(1:10, ~rnorm(30))

expect_mapping_names <- function(nms, .prt) {
  expect_true(all(.prt[["mapping_key"]][["variable"]] == nms))
}

expect_mapping_info <- function(info, .prt) {
  expected_info <- dplyr::near(.prt[["mapping_key"]][["information"]], info, tol = 1e-5)
  expect_true(all(expected_info))
}

expect_no_reduction <- function(.prt, .df) {
  all_1 <- all(.prt[["mapping_key"]][["information"]] == 1)
  expect_true(all_1)
  expect_equal(.prt[["mapping_key"]][["variable"]], names(.df))
}

test_that("part_icc() reduces correctly, high threshold", {
  prt <- partition(df, threshold = .6)
  expect_mapping_names(c("block1_x3", "block1_x5", "reduced_var_1"), prt)
  expect_mapping_info(c(1, 1, 0.6003696), prt)
})

test_that("part_icc() reduces correctly, low threshold", {
  prt <- partition(df, threshold = .1)
  expect_mapping_names("reduced_var_1", prt)
  expect_mapping_info(0.5532027, prt)
})

test_that("part_icc() reduces correctly, independent data", {
  ind_prt <- partition(ind_df, threshold = .8)
  expect_no_reduction(ind_prt, ind_df)
})

test_that("part_kmeans() reduces correctly, high threshold", {
  prt <- partition(df, threshold = .6, partitioner = part_kmeans())
  expect_mapping_names(c("reduced_var_1", "block1_x2", "block1_x3", "block1_x5"), prt)
  expect_mapping_info(c(0.6732932, 1, 1, 1), prt)
})

test_that("part_kmeans() reduces correctly, low threshold", {
  prt <- partition(df, threshold = .1, partitioner = part_kmeans())
  expect_mapping_names("reduced_var_1", prt)
  expect_mapping_info(0.5532027, prt)
})

test_that("part_kmeans() reduces correctly, independent data", {
  ind_prt <- partition(ind_df, threshold = .8, partitioner = part_kmeans())
  expect_no_reduction(ind_prt, ind_df)
})

test_that("part_minr2() reduces correctly, high threshold", {
  prt <- partition(df, threshold = .6, partitioner = part_minr2())
  expect_mapping_names(c("block1_x5", "reduced_var_1"), prt)
  expect_mapping_info(c(1, 0.6545881), prt)
})

test_that("part_minr2() reduces correctly, low threshold", {
  prt <- partition(df, threshold = .1, partitioner = part_minr2())
  expect_mapping_names("reduced_var_1", prt)
  expect_mapping_info(0.5661206, prt)
})

test_that("part_minr2() reduces correctly, independent data", {
  ind_prt <- partition(ind_df, threshold = .8, partitioner = part_minr2())
  expect_no_reduction(ind_prt, ind_df)
})

test_that("part_pc1() reduces correctly, high threshold", {
  prt <- partition(df, threshold = .7, partitioner = part_pc1())
  expect_mapping_names(c("reduced_var_1", "reduced_var_2"), prt)
  expect_mapping_info(c(0.733034068, 0.76641815), prt)
})

test_that("part_pc1() reduces correctly, low threshold", {
  prt <- partition(df, threshold = .1, partitioner = part_pc1())
  expect_mapping_names("reduced_var_1", prt)
  expect_mapping_info(0.641662, prt)
})

test_that("part_pc1() reduces correctly, independent data", {
  ind_prt <- partition(ind_df, threshold = .8, partitioner = part_pc1())
  expect_no_reduction(ind_prt, ind_df)
})

test_that("part_stdmi() reduces correctly, high threshold", {
  prt <- partition(df, threshold = .4, partitioner = part_stdmi())
  expect_mapping_names(c("block1_x2", "block1_x3", "block1_x5", "reduced_var_1"), prt)
  expect_mapping_info(c(1, 1, 1, 0.413647), prt)
})

test_that("part_stdmi() reduces correctly, low threshold", {
  prt <- partition(df, threshold = .1, partitioner = part_stdmi())
  expect_mapping_names("reduced_var_1", prt)
  expect_mapping_info(0.311516, prt)
})

test_that("part_stdmi() reduces correctly, independent data", {
  ind_prt <- partition(ind_df, threshold = .8, partitioner = part_stdmi())
  expect_no_reduction(ind_prt, ind_df)
})

test_that("pure data frames work as well", {
  ind_df <- as.data.frame(ind_df)
  subset_var_drop <- ind_df[, 2]
  subset_var_no_drop <- ind_df[, 2, drop = FALSE]

  expect_error(
    purrr::map(subset_var_drop, corr, ind_df$V1),
    "x and y are not the same length!"
  )

  expect_equal(
    purrr::map(subset_var_no_drop, corr, ind_df$V1),
    list(V2 = -0.1742284),
    tolerance = 1e-05
  )

  prt_tbl <- partition(df, threshold = .1)
  prt_df <- partition(as.data.frame(df), threshold = .1)
  expect_identical(prt_tbl$reduced_data, prt_df$reduced_data)
  expect_identical(prt_tbl$mapping_key, prt_df$mapping_key)
  expect_identical(prt_tbl$threshold, prt_df$threshold)
  expect_identical(prt_tbl$partitioner, prt_df$partitioner)

  ind_prt <- partition(ind_df, threshold = .8)
  expect_no_reduction(ind_prt, ind_df)
})
