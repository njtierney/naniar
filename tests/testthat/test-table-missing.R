context("table_missing_* tidiers")

test_that("table_missing_* errors on NULL",{
  expect_error(miss_var_table(NULL))
  expect_error(miss_case_table(NULL))
})

test_that("table_missing_* errors when a non-dataframe given",{
  expect_error(miss_var_table(1))
  expect_error(miss_var_table("a"))
  expect_error(miss_var_table(matrix(iris)))

  expect_error(miss_case_table(1))
  expect_error(miss_case_table("a"))
  expect_error(miss_case_table(matrix(iris)))
})

test_that("table_missing_* produces a data_frame", {
  expect_is(miss_var_table(airquality), "tbl_df")
  expect_is(miss_case_table(airquality), "tbl_df")
})


