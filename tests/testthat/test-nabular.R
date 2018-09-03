context("test-nabular")


test_that("nabular returns a data.frame",{
  expect_s3_class(nabular(airquality),
                  "data.frame")
})

test_that("nabular returns a tibble",{
  expect_equal(class(nabular(airquality)),
               c("nabular", "tbl_df", "tbl", "data.frame"))
})

test_that("nabular errors when given non dataframe or 0 entry",{
  expect_error(nabular(0))
  expect_error(nabular("a"))
  expect_error(nabular(matrix(airquality)))
  expect_error(nabular(NULL))
})

test_that("new_nabular errors when given non dataframe or 0 entry",{
  expect_error(new_nabular(0))
  expect_error(new_nabular(NULL))
})
