test_that("miss-var-which returns the right variables", {
  expect_equal(miss_var_which(airquality),
               c("Ozone", "Solar.R"))
})

test_that("miss-var-which errors on NULL", {
  expect_error(miss_var_which(NULL))
})

test_that("miss-var-which returns null when no missings", {
  expect_null(miss_var_which(iris))
})
