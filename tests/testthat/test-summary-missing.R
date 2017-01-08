context("summary_missing_* tidiers")

test_that("summary_missing_* errors on NULL",{
  expect_error(miss_var_summary(NULL))
  expect_error(miss_case_summary(NULL))
})

test_that("summary_missing_* errors when a non-dataframe given",{
  expect_error(miss_var_summary(1))
  expect_error(miss_var_summary("a"))
  expect_error(miss_var_summary(matrix(iris)))

  expect_error(miss_case_summary(1))
  expect_error(miss_case_summary("a"))
  expect_error(miss_case_summary(matrix(iris)))
})

test_that("summary_missing_* produces a data_frame", {
  expect_is(miss_var_summary(airquality), "tbl_df")
  expect_is(miss_case_summary(airquality), "tbl_df")
})
