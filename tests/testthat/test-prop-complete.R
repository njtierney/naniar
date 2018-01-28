context("prop_complete")

test_that("prop_complete handles 0 cases as I expect",{

  expect_equal(prop_complete(0), 1)
  expect_equal(prop_complete(TRUE), 1)
  expect_equal(prop_complete("TRUE"), 1)
  expect_equal(prop_complete(numeric(0)),NaN)

  expect_equal(prop_complete(iris[0]),NaN)

})

test_that("prop_complete correctly counts the proportion of missings",{
  expect_equal(prop_complete(c(0,NA,120,NA)),0.5)
  expect_equal(prop_complete(c(NA,NA,120,NA)),0.25)
  expect_equal(prop_complete(c(NA,NA,NA,NA)),0)
  expect_equal(prop_complete(c(1,2,3,4,5)),1)
})

test_that("prop_complete works for dataframes",{
  expect_true(dplyr::near(round(prop_complete(airquality), 5), 0.95207))
})
