context("miss_*_pct tidiers")

test_that("miss_*_pct errors on NULL",{
  expect_error(miss_df_pct(NULL))
  expect_error(miss_var_pct(NULL))
  expect_error(miss_case_pct(NULL))
})

test_that("miss_*_pct errors when a non-dataframe given",{
  expect_error(miss_df_pct(1))
  expect_error(miss_df_pct("a"))
  expect_error(miss_df_pct(matrix(iris)))

  expect_error(miss_var_pct(1))
  expect_error(miss_var_pct("a"))
  expect_error(miss_var_pct(matrix(iris)))

  expect_error(miss_case_pct(1))
  expect_error(miss_case_pct("a"))
  expect_error(miss_case_pct(matrix(iris)))
})

test_that("miss_*_pct produces a single, numeric number", {

  expect_length(miss_df_pct(airquality), 1)
  expect_type(miss_df_pct(airquality), "double")

  expect_length(miss_var_pct(airquality), 1)
  expect_type(miss_var_pct(airquality), "double")

  expect_length(miss_case_pct(airquality), 1)
  expect_type(miss_case_pct(airquality), "double")
})
