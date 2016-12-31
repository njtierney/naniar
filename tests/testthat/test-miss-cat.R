context("miss_cat")

mc_result <- miss_cat(airquality,
                      "Solar.R",
                      "Ozone")

test_that("the length of miss_cat is what we expect",{
  expect_equal(length(mc_result),nrow(airquality))
})

test_that("miss_cat produces character vector",{
  expect_is(mc_result,"character")
})

test_that("miss_cat errors when dataframe isn't provided",{
  expect_error(miss_cat(matrix(iris),"Sepal.Length","Sepal.Width"))
})

test_that("miss_cat errors when character string isn't provided",{
  expect_error(miss_cat(matrix(iris),2,3))
})
