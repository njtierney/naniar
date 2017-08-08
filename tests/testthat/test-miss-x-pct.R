context("na_*_pct tidiers")

test_that("na_*_pct errors on NULL",{
  expect_error(prop_na(NULL))
  expect_error(na_var_pct(NULL))
  expect_error(na_case_pct(NULL))
})

test_that("na_*_pct errors when a non-dataframe given",{
  expect_error(na_var_pct(1))
  expect_error(na_var_pct("a"))
  expect_error(na_var_pct(matrix(iris)))

  expect_error(na_case_pct(1))
  expect_error(na_case_pct("a"))
  expect_error(na_case_pct(matrix(iris)))
})

test_that("na_*_pct produces a single, numeric number", {

  expect_length(prop_na(airquality), 1)
  expect_type(prop_na(airquality), "double")

  expect_length(na_var_pct(airquality), 1)
  expect_type(na_var_pct(airquality), "double")

  expect_length(na_case_pct(airquality), 1)
  expect_type(na_case_pct(airquality), "double")
})
