context("n_na")

test_that("n_na handles 0 cases as I expect",{

  expect_equal(n_na(0), 0)

  expect_equal(n_na(numeric(0)),0)

  expect_equal(n_na(iris[0]),0)

})

test_that("n_na correctly counts the number of missings",{
  expect_equal(n_na(c(0,NA,120,NA)),2)
  expect_equal(n_na(c(NA,NA,NA,NA)),4)
  expect_equal(n_na(c(1,2,3,4,5)),0)
})

test_that("n_na works for dataframes",{
  expect_equal(n_na(airquality), 44)
})
