context("miss_case_summary tidiers")

test_that("miss_case_summary errors on NULL",{
  expect_error(miss_case_summary(NULL))
})

test_that("miss_case_summary errors when a non-dataframe given",{

  expect_error(miss_case_summary(1))
  expect_error(miss_case_summary("a"))
  expect_error(miss_case_summary(matrix(iris)))
})

test_that("miss_case_summary produces a data_frame", {
  expect_is(miss_case_summary(airquality), "tbl_df")
})

# grouping

aq_group <- dplyr::group_by(airquality, Month)

test_that("miss_case_summary on grouped_df returns a tibble", {
  expect_is(miss_case_summary(aq_group), "tbl_df")
})

test_that("grouped_df returns 1 more column than regular miss_case_summary", {
  expect_equal(ncol(miss_case_summary(aq_group)),
                   ncol(miss_case_summary(airquality))+1)
})

test_that("grouped_df returns a column named 'Month'", {
  expect_identical(names(miss_case_summary(aq_group)),
                   c("Month", "case", "n_miss","pct_miss", "n_miss_cumsum"))
})

test_that("grouped_df returns a column named 'Month' with the right levels", {
  expect_identical(unique(miss_case_summary(aq_group)$Month),
                   5:9)
})

