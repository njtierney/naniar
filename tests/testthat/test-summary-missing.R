context("summary_missing_* tidiers")

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
