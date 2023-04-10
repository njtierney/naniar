test_that("n_miss handles 0 cases as I expect",{

  expect_equal(n_miss(0), 0)
  expect_equal(n_miss(numeric(0)),0)
  expect_equal(n_miss(mtcars[0]),0)

})

test_that("n_miss correctly counts the number of missings",{
  expect_equal(n_miss(c(0,NA,120,NA)),2)
  expect_equal(n_miss(c(NA,NA,NA,NA)),4)
  expect_equal(n_miss(c(1,2,3,4,5)),0)
})

test_that("n_miss works for dataframes",{
  expect_equal(n_miss(airquality), 44)
})
