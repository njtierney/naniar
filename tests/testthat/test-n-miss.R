context("n_miss")

test_that("n_miss handles 0 cases as I expect",{

  expect_equal(n_miss(0), 0)

  expect_equal(n_miss(numeric(0)),0)

  expect_equal(n_miss(iris[0]),0)

})
