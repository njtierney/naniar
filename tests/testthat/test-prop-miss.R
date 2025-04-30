test_that("prop_miss handles 0 cases as I expect", {
  expect_equal(prop_miss(0), 0)
  expect_equal(prop_miss(TRUE), 0)
  expect_equal(prop_miss(numeric(0)), NaN)

  expect_equal(prop_miss(iris[0]), NaN)
})

test_that("prop_miss correctly counts the proportion of missings", {
  expect_equal(prop_miss(c(0, NA, 120, NA)), 0.5)
  expect_equal(prop_miss(c(NA, NA, 120, NA)), 0.75)
  expect_equal(prop_miss(c(NA, NA, NA, NA)), 1)
  expect_equal(prop_miss(c(1, 2, 3, 4, 5)), 0)
})

test_that("prop_miss works for dataframes", {
  expect_true(dplyr::near(round(prop_miss(airquality), 5), 0.047930))
})
