context("prop_na")

test_that("prop_na returns a data.frame",{
  expect_s3_class(prop_na(airquality),
                  "data.frame")
})

test_that("prop_na returns a tibble",{
  expect_equal(class(prop_na(airquality)),
               c("tbl_df", "tbl", "data.frame"))
})

test_that("prop_na errors when given non dataframe or 0 entry",{
  expect_error(prop_na(0))
  expect_error(prop_na("a"))
  expect_error(prop_na(matrix(airquality)))
  expect_error(prop_na(NULL))
})

test_that("There are 3 columns",{
  expect_equal(ncol(prop_na(airquality)),3)
})

test_that("The columns are named df, var, case",{
  expect_named(prop_na(airquality),c("df","var","case"))
})
