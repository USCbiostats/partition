set.seed(1234)

expect_mapping_names <- function(nms, .prt) {
  expect_true(all(.prt[["mapping_key"]][["variable"]] == nms))
}

expect_mapping_info <- function(info, .prt) {
  expected_info <- dplyr::near(round(.prt[["mapping_key"]][["information"]], 3), info, tol = .001)
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
  expect_mapping_info(c(1, 1, 0.600), prt)
})

test_that("part_icc() reduces correctly, low threshold", {
  prt <- partition(df, threshold = .1)
  expect_mapping_names("reduced_var_1", prt)
  expect_mapping_info(0.553, prt)
})

test_that("part_icc() reduces correctly, independent data", {
  ind_prt <- partition(ind_df, threshold = .8)
  expect_no_reduction(ind_prt, ind_df)
})

test_that("part_kmeans() reduces correctly, high threshold", {
  prt <- partition(df, threshold = .6, partitioner = part_kmeans())
  expect_mapping_names(c("reduced_var_1", "block1_x2", "block1_x3", "block1_x5"), prt)
  expect_mapping_info(c(0.673, 1, 1, 1), prt)
})

test_that("part_kmeans() reduces correctly, low threshold", {
  prt <- partition(df, threshold = .1, partitioner = part_kmeans())
  expect_mapping_names("reduced_var_1", prt)
  expect_mapping_info(0.553, prt)
})

test_that("part_kmeans() reduces correctly, independent data", {
  ind_prt <- partition(ind_df, threshold = .8, partitioner = part_kmeans())
  expect_no_reduction(ind_prt, ind_df)
})

test_that("part_minr2() reduces correctly, high threshold", {
  prt <- partition(df, threshold = .6, partitioner = part_minr2())
  expect_mapping_names(c("block1_x5", "reduced_var_1"), prt)
  expect_mapping_info(c(1, 0.655), prt)
})

test_that("part_minr2() reduces correctly, low threshold", {
  prt <- partition(df, threshold = .1, partitioner = part_minr2())
  expect_mapping_names("reduced_var_1", prt)
  expect_mapping_info(0.566, prt)
})

test_that("part_minr2() reduces correctly, independent data", {
  ind_prt <- partition(ind_df, threshold = .8, partitioner = part_minr2())
  expect_no_reduction(ind_prt, ind_df)
})

test_that("part_pc1() reduces correctly, high threshold", {
  prt <- partition(df, threshold = .7, partitioner = part_pc1())
  expect_mapping_names(c("reduced_var_1", "reduced_var_2"), prt)
  expect_mapping_info(c(0.733, 0.766), prt)
})

test_that("part_pc1() reduces correctly, low threshold", {
  prt <- partition(df, threshold = .1, partitioner = part_pc1())
  expect_mapping_names("reduced_var_1", prt)
  expect_mapping_info(0.642, prt)
})

test_that("part_pc1() reduces correctly, independent data", {
  ind_prt <- partition(ind_df, threshold = .8, partitioner = part_pc1())
  expect_no_reduction(ind_prt, ind_df)
})

test_that("part_stdmi() reduces correctly, high threshold", {
  prt <- partition(df, threshold = .4, partitioner = part_stdmi())
  expect_mapping_names(c("block1_x2", "block1_x3", "block1_x5", "reduced_var_1"), prt)
  expect_mapping_info(c(1, 1, 1, 0.414), prt)
})

test_that("part_stdmi() reduces correctly, low threshold", {
  prt <- partition(df, threshold = .1, partitioner = part_stdmi())
  expect_mapping_names("reduced_var_1", prt)
  expect_mapping_info(0.312, prt)
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
    purrr::map(subset_var_drop, corr, ind_df$V1)
  )

  expect_equal(
    purrr::map(subset_var_no_drop, corr, ind_df$V1) %>%
      purrr::map(round, 3),
    list(V2 = -0.056),
    tolerance = .001
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

test_that("Data with missing values still reduces", {
  # set random values to NA
  df[sample(seq_len(nrow(df)), size = 10), sample(seq_len(ncol(df)), size = 5)] <- NA

  prt_icc <- partition(df, threshold = .6)
  prt_kmeans <- partition(df, threshold = .6, part_kmeans())
  prt_mi <- partition(df, threshold = .6, part_stdmi())
  prt_pc1 <- partition(df, threshold = .6, part_pc1())
  prt_minr2 <- partition(df, threshold = .6, part_minr2())

  expect_s3_class(prt_icc, "partition")
  expect_s3_class(prt_kmeans, "partition")
  expect_s3_class(prt_mi, "partition")
  expect_s3_class(prt_pc1, "partition")
  expect_s3_class(prt_minr2, "partition")

  # expect same number of rows as `df`
  expect_equal(nrow(partition_scores(prt_icc)), nrow(df))
  expect_equal(nrow(partition_scores(prt_kmeans)), nrow(df))
  expect_equal(nrow(partition_scores(prt_mi)), nrow(df))
  expect_equal(nrow(partition_scores(prt_pc1)), nrow(df))
  expect_equal(nrow(partition_scores(prt_minr2)), nrow(df))
})
