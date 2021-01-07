set.seed(1234)
df <- simulate_block_data(5, lower_corr = .6, upper_corr = .65, n = 100)

test_that("simulate_block_data() works", {
  blocks <- c(3, 4, 5)
  n <- 100
  expect_silent(df2 <- simulate_block_data(blocks, lower_corr = .4, upper_corr = .6, n = n))
  expect_s3_class(df2, "tbl")
  expect_length(df2, sum(blocks))
  expect_equal(nrow(df2), n)
})

df <- simulate_block_data(5, lower_corr = .6, upper_corr = .65, n = 100)

test_that("all partitioners are partitioners", {
  expect_s3_class(part_icc(), "partitioner")
  expect_s3_class(part_kmeans(), "partitioner")
  expect_s3_class(part_minr2(), "partitioner")
  expect_s3_class(part_pc1(), "partitioner")
  expect_s3_class(part_stdmi(), "partitioner")
})

test_that("custom partitioners are partitioners", {
  prtnr <- as_partitioner(
    direct = direct_distance_spearman,
    measure = measure_min_icc,
    reduce = as_reducer(rowMeans)
  )

  expect_s3_class(prtnr, "partitioner")
})

test_that("custom metric works", {
  inter_item_reliability <- function(.data) {
    corr(.data) %>%
      colMeans(na.rm = TRUE) %>%
      mean()
  }

  part_iir <- replace_partitioner(
    part_icc,
    measure = as_measure(inter_item_reliability)
  )

  prt <- partition(df, .8, partitioner = part_iir)
  expect_s3_class(prt, "partition")
})

test_that("custom director works", {
  euc_dist <- function(.data) as.matrix(dist(t(.data)))

  min_dist <- function(.x) {
    indices <- arrayInd(which.min(.x), dim(as.matrix(.x)))

    #  get variable names with minimum distance
    c(
      colnames(.x)[indices[1]],
      colnames(.x)[indices[2]]
    )
  }

  direct_euc_dist <- as_director(euc_dist, min_dist)

  part_euc_dist <- replace_partitioner(
    part_icc,
    direct = as_director(direct_euc_dist)
  )

  prt <- partition(
    df,
    .4,
    partitioner = replace_partitioner(part_icc(), direct = as_director(euc_dist, min_dist))
  )
  expect_s3_class(prt, "partition")
})

test_that("custom reducer works", {
  part_rowmeans <- replace_partitioner(
    part_icc,
    reduce = as_reducer(rowMeans)
  )

  prt <- partition(df, .65, partitioner = part_rowmeans)
  expect_s3_class(prt, "partition")
})
