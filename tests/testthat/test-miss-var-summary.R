context("miss_var_summary tidiers")

test_that("miss_var_summary errors on NULL",{
  expect_error(miss_var_summary(NULL))
})

test_that("miss_var_summary errors when a non-dataframe given",{
  expect_error(miss_var_summary(1))
  expect_error(miss_var_summary("a"))
  expect_error(miss_var_summary(matrix(iris)))
})

test_that("miss_var_summary produces a data_frame", {
  expect_is(miss_var_summary(airquality), "tbl_df")
})
