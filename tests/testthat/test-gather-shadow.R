context("gather_shadow")

test_that("gather_shadow returns a data.frame",{
  expect_s3_class(gather_shadow(airquality),
                  "data.frame")
})

test_that("gather_shadow returns a tibble",{
  expect_equal(class(gather_shadow(airquality)),
               c("tbl_df", "tbl", "data.frame"))
})

test_that("gather_shadow errors when given non dataframe or 0 entry",{
  expect_error(gather_shadow(0))
  expect_error(gather_shadow("a"))
  expect_error(gather_shadow(matrix(airquality)))
  expect_error(gather_shadow(NULL))
})

test_that("The number of rows are the same after using gather_shadow",{
  expect_equal(nrow(gather_shadow(airquality)),nrow(airquality)*6)
})

test_that("There are three columns in gather_shadow",{
  expect_equal(ncol(gather_shadow(airquality)),3)
})

test_that("gather_shadow returns columns with right names",{
  expect_equal(names(gather_shadow(airquality)),
               c("case","variable","missing"))
})
