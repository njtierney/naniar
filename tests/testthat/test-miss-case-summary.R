context("miss_case_summary tidiers")

test_that("miss_case_summary errors on NULL",{
  expect_error(miss_case_summary(NULL))
})

test_that("miss_case_summary errors when a non-dataframe given",{

  expect_error(miss_case_summary(1))
  expect_error(miss_case_summary("a"))
  expect_error(miss_case_summary(matrix(iris)))
})

test_that("miss_case_summary produces a tibble", {
  expect_is(miss_case_summary(airquality), "tbl_df")
})

# grouping

aq_group <- dplyr::group_by(airquality, Month)

test_that("miss_case_summary on grouped_df returns a tibble", {
  expect_is(miss_case_summary(aq_group), "tbl_df")
})

test_that("miss_case_summary produces the right number of columns", {
  expect_equal(ncol(miss_case_summary(airquality)),
               3)
})

test_that("miss_case_summary grouped produces the right number of columns", {
  expect_equal(ncol(miss_case_summary(aq_group)),
               4)
})

test_that("grouped_df returns the same number of columns as regular miss_case_summary", {
  expect_equal(ncol(miss_case_summary(aq_group)),
               ncol(miss_case_summary(airquality)) + 1)
})

test_that("grouped_df returns a column named 'Month'", {
  expect_identical(names(miss_case_summary(aq_group)),
                   c("Month", "case", "n_miss","pct_miss"))
})

test_that("grouped_df returns a column named 'Month' with the right levels", {
  expect_identical(unique(miss_case_summary(aq_group)$Month),
                   5:9)
})

# add testing for cumulative sum ----------------------------------------------

test_that("miss_case_summary adds cumsum when add_cumsum = TRUE", {
  expect_equal(names(miss_case_summary(airquality, add_cumsum = TRUE)),
               c("case", "n_miss", "pct_miss", "n_miss_cumsum"))
})

test_that("miss_case_summary grouped adds cumsum when add_cumsum = TRUE", {
  expect_equal(names(miss_case_summary(aq_group, add_cumsum = TRUE)),
               c("Month", "case", "n_miss", "pct_miss", "n_miss_cumsum"))
})
