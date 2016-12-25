context("missingness_tidiers")

test_that("percent_missing_* produces a single, numeric number", {

  expect_length(percent_missing_df(airquality), 1)
  expect_type(percent_missing_df(airquality), "double")

  expect_length(percent_missing_var(airquality), 1)
  expect_type(percent_missing_var(airquality), "double")

  expect_length(percent_missing_case(airquality), 1)
  expect_type(percent_missing_case(airquality), "double")
})

test_that("table_missing_* produces a data_frame", {
  expect_is(table_missing_var(airquality), "tbl_df")
  expect_is(table_missing_case(airquality), "tbl_df")
})


