context("n_complete")

test_that("n_complete handles 0 cases as I expect",{

  expect_equal(n_complete(0), 1)

  expect_equal(n_complete(numeric(0)),0)

  expect_equal(n_complete(iris[0]),0)

})

test_that("n_complete correctly counts the number of missings",{
  expect_equal(n_complete(c(0,NA,120,NA)),2)
  expect_equal(n_complete(c(NA,NA,NA,NA)),0)
  expect_equal(n_complete(c(1,2,3,4,5)),5)
})

test_that("n_complete works for dataframes",{
  expect_equal(n_complete(airquality), 874)
})
