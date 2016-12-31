context("bind_shadow")

test_that("bind_shadow returns a data.frame",{
  expect_s3_class(bind_shadow(airquality),
                  "data.frame")
})

test_that("bind_shadow returns a tibble",{
  expect_equal(class(bind_shadow(airquality)),
               c("tbl_df", "tbl", "data.frame"))
})

test_that("bind_shadow errors when given non dataframe or 0 entry",{
  expect_error(bind_shadow(0))
  expect_error(bind_shadow("a"))
  expect_error(bind_shadow(matrix(airquality)))
  expect_error(bind_shadow(NULL))
})

test_that("The number of rows are the same after using bind_shadow",{
  expect_equal(nrow(bind_shadow(airquality)),nrow(airquality))
})

test_that("The number of columns are twice the number of bind_shadow",{
  expect_equal(ncol(bind_shadow(airquality)),ncol(airquality)*2)
})

test_that("bind_shadow returns columns with additional suffix _NA",{
  expect_equal(names(bind_shadow(airquality)),
               c(names(airquality),
                 paste0(names(airquality),"_NA")))
})

