context("bind_shadow")

test_that("bind_shadow returns a data.frame",{
  expect_s3_class(bind_shadow(airquality),
                  "data.frame")
})

test_that("bind_shadow returns a tibble",{
  expect_equal(class(bind_shadow(airquality)),
               c("nabular", "tbl_df", "tbl", "data.frame"))
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

test_that("bind_shadow with only_miss = TRUE returns smaller dataframe",{
  expect_gt(
    ncol(bind_shadow(airquality)),
    ncol(bind_shadow(airquality, only_miss = TRUE))
  )
})

test_that("bind_shadow with only_miss = TRUE returns the right number of cols",{
  expect_equal(
    ncol(bind_shadow(airquality, only_miss = TRUE)),
    ncol(airquality) + 2
    )
})

test_that(
  "bind_shadow with only_miss = TRUE returns columns with extra suffix _NA",{
  expect_equal(names(bind_shadow(airquality, only_miss = TRUE)),
               c(names(airquality),
                 paste0(miss_var_which(airquality),"_NA")))
})
