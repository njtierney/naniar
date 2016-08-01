context("missingness_tidiers")

test_that("percent_missing_* produces a single, numeric number", {
  expect_length(ggmissing::percent_missing_df(airquality), 1)
  testthat::expect_type(ggmissing::percent_missing_df(airquality), "double")
  expect_length(ggmissing::percent_missing_var(airquality), 1)
  testthat::expect_type(ggmissing::percent_missing_var(airquality), "double")
  expect_length(ggmissing::percent_missing_case(airquality), 1)
  testthat::expect_type(ggmissing::percent_missing_case(airquality), "double")
})

test_that("table_missing_* produces a data_frame", {
  testthat::expect_is(ggmissing::table_missing_var(airquality), "tbl_df")
  testthat::expect_is(ggmissing::table_missing_case(airquality), "tbl_df")
})


