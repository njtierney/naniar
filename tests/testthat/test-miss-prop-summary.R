context("miss_prop_summary")

test_that("miss_prop_summary returns a data.frame",{
  expect_s3_class(miss_prop_summary(airquality),
                  "data.frame")
})

test_that("miss_prop_summary returns a tibble",{
  expect_equal(class(miss_prop_summary(airquality)),
               c("tbl_df", "tbl", "data.frame"))
})

test_that("miss_prop_summary errors when given non dataframe or 0 entry",{
  expect_error(miss_prop_summary(0))
  expect_error(miss_prop_summary("a"))
  expect_error(miss_prop_summary(matrix(airquality)))
  expect_error(miss_prop_summary(NULL))
})

test_that("There are 3 columns",{
  expect_equal(ncol(miss_prop_summary(airquality)),3)
})

test_that("The columns are named df, var, case",{
  expect_named(miss_prop_summary(airquality),c("df","var","case"))
})
