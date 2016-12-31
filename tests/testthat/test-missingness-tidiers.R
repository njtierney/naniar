context("missingness_tidiers")


test_that("percent_missing_* errors on NULL",{
  expect_error(percent_missing_df(NULL))
  expect_error(percent_missing_var(NULL))
  expect_error(percent_missing_case(NULL))
})

test_that("percent_missing_* errors when a non-dataframe given",{
  expect_error(percent_missing_df(1))
  expect_error(percent_missing_df("a"))
  expect_error(percent_missing_df(matrix(iris)))

  expect_error(percent_missing_var(1))
  expect_error(percent_missing_var("a"))
  expect_error(percent_missing_var(matrix(iris)))

  expect_error(percent_missing_case(1))
  expect_error(percent_missing_case("a"))
  expect_error(percent_missing_case(matrix(iris)))
})

test_that("percent_missing_* produces a single, numeric number", {

  expect_length(percent_missing_df(airquality), 1)
  expect_type(percent_missing_df(airquality), "double")

  expect_length(percent_missing_var(airquality), 1)
  expect_type(percent_missing_var(airquality), "double")

  expect_length(percent_missing_case(airquality), 1)
  expect_type(percent_missing_case(airquality), "double")
})


test_that("summary_missing_* errors on NULL",{
  expect_error(summary_missing_df(NULL))
  expect_error(summary_missing_var(NULL))
  expect_error(summary_missing_case(NULL))
})

test_that("summary_missing_* errors when a non-dataframe given",{
  expect_error(summary_missing_var(1))
  expect_error(summary_missing_var("a"))
  expect_error(summary_missing_var(matrix(iris)))

  expect_error(summary_missing_case(1))
  expect_error(summary_missing_case("a"))
  expect_error(summary_missing_case(matrix(iris)))
})

test_that("summary_missing_* produces a data_frame", {
  expect_is(summary_missing_var(airquality), "tbl_df")
  expect_is(summary_missing_case(airquality), "tbl_df")
})


test_that("table_missing_* errors on NULL",{
  expect_error(table_missing_df(NULL))
  expect_error(table_missing_var(NULL))
  expect_error(table_missing_case(NULL))
})

test_that("table_missing_* errors when a non-dataframe given",{
  expect_error(table_missing_var(1))
  expect_error(table_missing_var("a"))
  expect_error(table_missing_var(matrix(iris)))

  expect_error(table_missing_case(1))
  expect_error(table_missing_case("a"))
  expect_error(table_missing_case(matrix(iris)))
})

test_that("table_missing_* produces a data_frame", {
  expect_is(table_missing_var(airquality), "tbl_df")
  expect_is(table_missing_case(airquality), "tbl_df")
})


