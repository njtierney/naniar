context("missingness scalar summaries")

test_that("prop_miss* errors on NULL",{
  expect_error(prop_miss(NULL))
  expect_error(prop_miss_var(NULL))
  expect_error(prop_miss_case(NULL))
})

test_that("pct_miss* errors on NULL",{
  expect_error(pct_miss(NULL))
  expect_error(pct_miss_var(NULL))
  expect_error(pct_miss_case(NULL))
})

test_that("prop_complete* errors on NULL",{
  expect_error(prop_complete(NULL))
  expect_error(prop_complete_var(NULL))
  expect_error(prop_complete_case(NULL))
})

test_that("pct_complete* errors on NULL",{
  expect_error(pct_complete(NULL))
  expect_error(pct_complete_var(NULL))
  expect_error(pct_complete_case(NULL))
})

test_that("prop_miss* errors when a non-dataframe given",{
  expect_error(prop_miss_var(1))
  expect_error(prop_miss_var("a"))
  expect_error(prop_miss_var(matrix(iris)))

  expect_error(prop_miss_case(1))
  expect_error(prop_miss_case("a"))
  expect_error(prop_miss_case(matrix(iris)))
})

test_that("prop_complete* errors when a non-dataframe given",{
  expect_error(prop_complete_var(1))
  expect_error(prop_complete_var("a"))
  expect_error(prop_complete_var(matrix(iris)))

  expect_error(prop_complete_case(1))
  expect_error(prop_complete_case("a"))
  expect_error(prop_complete_case(matrix(iris)))
})
test_that("pct_miss* errors when a non-dataframe given",{
  expect_error(pct_miss_var(1))
  expect_error(pct_miss_var("a"))
  expect_error(pct_miss_var(matrix(iris)))

  expect_error(pct_miss_case(1))
  expect_error(pct_miss_case("a"))
  expect_error(pct_miss_case(matrix(iris)))
})

test_that("pct_complete* errors when a non-dataframe given",{
  expect_error(pct_complete_var(1))
  expect_error(pct_complete_var("a"))
  expect_error(pct_complete_var(matrix(iris)))

  expect_error(pct_complete_case(1))
  expect_error(pct_complete_case("a"))
  expect_error(pct_complete_case(matrix(iris)))
})

test_that("missingness scalar summaries produce a single, numeric number", {

  expect_length(prop_miss(airquality), 1)
  expect_type(prop_miss(airquality), "double")

  expect_length(pct_miss(airquality), 1)
  expect_type(pct_miss(airquality), "double")

  expect_length(prop_miss_var(airquality), 1)
  expect_type(prop_miss_var(airquality), "double")

  expect_length(pct_miss_var(airquality), 1)
  expect_type(pct_miss_var(airquality), "double")

  expect_length(prop_miss_case(airquality), 1)
  expect_type(prop_miss_case(airquality), "double")

  expect_length(pct_miss_case(airquality), 1)
  expect_type(pct_miss_case(airquality), "double")

  expect_length(prop_complete(airquality), 1)
  expect_type(prop_complete(airquality), "double")

  expect_length(pct_complete(airquality), 1)
  expect_type(pct_complete(airquality), "double")

  expect_length(prop_complete_var(airquality), 1)
  expect_type(prop_complete_var(airquality), "double")

  expect_length(pct_complete_var(airquality), 1)
  expect_type(pct_complete_var(airquality), "double")

  expect_length(prop_complete_case(airquality), 1)
  expect_type(prop_complete_case(airquality), "double")

  expect_length(pct_complete_case(airquality), 1)
  expect_type(pct_complete_case(airquality), "double")

})
