test_that("miss_case_summary errors on NULL", {
  expect_snapshot(
    error = TRUE,
    miss_case_summary(NULL)
  )
})

test_that("miss_case_summary errors when a non-dataframe given", {
  expect_snapshot(
    error = TRUE,
    miss_case_summary(1)
  )
  expect_snapshot(
    error = TRUE,
    miss_case_summary("a")
  )
  expect_snapshot(
    error = TRUE,
    miss_case_summary(matrix(0))
  )
})

dat <- tibble::tribble(
  ~air,
  ~wind,
  ~water,
  ~month,
  -99,
  NA,
  23,
  1,
  -98,
  NA,
  NA,
  1,
  25,
  30,
  21,
  2,
  NA,
  99,
  NA,
  2,
  23,
  40,
  NA,
  2
)

test_that("miss_case_summary produces a tibble", {
  expect_s3_class(miss_case_summary(dat), "tbl_df")
})

# grouping

aq_group <- dplyr::group_by(dat, month)

test_that("miss_case_summary on grouped_df returns a tibble", {
  expect_s3_class(miss_case_summary(aq_group), "tbl_df")
})

test_that("miss_case_summary produces the right number of columns", {
  expect_equal(ncol(miss_case_summary(dat)), 3)
})

test_that("miss_case_summary grouped produces the right number of columns", {
  expect_equal(ncol(miss_case_summary(aq_group)), 4)
})

test_that("grouped_df returns the same number of columns as regular miss_case_summary", {
  expect_equal(
    ncol(miss_case_summary(aq_group)),
    ncol(miss_case_summary(dat)) + 1
  )
})

test_that("grouped_df returns a column named 'Month'", {
  expect_identical(
    names(miss_case_summary(aq_group)),
    c("month", "case", "n_miss", "pct_miss")
  )
})

test_that("grouped_df returns a column named 'Month' with the right levels", {
  expect_identical(
    unique(miss_case_summary(aq_group)$month),
    c(1, 2)
  )
})

# add testing for cumulative sum ----------------------------------------------

test_that("miss_case_summary adds cumsum when add_cumsum = TRUE", {
  expect_equal(
    names(miss_case_summary(dat, add_cumsum = TRUE)),
    c("case", "n_miss", "pct_miss", "n_miss_cumsum")
  )
})

test_that("miss_case_summary grouped adds cumsum when add_cumsum = TRUE", {
  expect_equal(
    names(miss_case_summary(aq_group, add_cumsum = TRUE)),
    c("month", "case", "n_miss", "pct_miss", "n_miss_cumsum")
  )
})
