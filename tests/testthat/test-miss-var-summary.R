dat <- tibble::tribble(
  ~air, ~wind, ~water, ~month,
  -99,    NA,  23,  1,
  -98,    NA,  NA,  1,
  25,     30,  21,  2,
  NA,     99,  NA,  2,
  23,     40,  NA,  2
)

test_that("miss_var_summary errors when a non-dataframe given",{
  expect_error(miss_var_summary(NULL))
  expect_error(miss_var_summary(matrix(0)))
})

test_that("miss_var_summary produces a tibble", {
  expect_s3_class(miss_var_summary(dat), "tbl_df")
})

# group_by testing -------------------------------------------------------------

aq_group <- dplyr::group_by(dat, month)

test_that("miss_var_summary grouped_df returns a tibble", {
  expect_s3_class(miss_var_summary(aq_group), "tbl_df")
})

test_that("miss_var_summary returns the right number of columns", {
  expect_equal(ncol(miss_var_summary(dat)), 3)
})

test_that("miss_var_summary group returns the right number of columns", {
  expect_equal(ncol(miss_var_summary(aq_group)), 4)
})

test_that("grouped_df returns 1 more column than regular miss_var_summary", {
  expect_equal(ncol(miss_var_summary(aq_group)),
               ncol(miss_var_summary(dat)) + 1)
})

test_that("grouped_df returns a column named 'Month'", {
  expect_identical(names(miss_var_summary(aq_group)),
                   c("month", "variable", "n_miss","pct_miss"))
})

test_that("grouped_df returns a dataframe with more rows than regular", {
  expect_gt(nrow(miss_var_summary(aq_group)),
            nrow(miss_var_summary(dat)))
})

test_that("grouped_df returns a column named 'Month' with the right levels", {
  expect_identical(unique(miss_var_summary(aq_group)$month),
                   c(1,2))
})



# add testing for cumulative sum ----------------------------------------------

test_that("miss_var_summary adds cumsum when add_cumsum = TRUE", {
  expect_equal(names(miss_var_summary(dat, add_cumsum = TRUE)),
               c("variable", "n_miss", "pct_miss", "n_miss_cumsum"))
})

test_that("miss_var_summary grouped adds cumsum when add_cumsum = TRUE", {
  expect_equal(names(miss_var_summary(aq_group, add_cumsum = TRUE)),
               c("month", "variable", "n_miss", "pct_miss", "n_miss_cumsum"))
})
