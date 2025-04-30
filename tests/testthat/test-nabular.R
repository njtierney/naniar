test_that("nabular and bind_shadow return the same output", {
  expect_equal(nabular(airquality), bind_shadow(airquality))
})

test_that("nabular returns a data.frame or tbl", {
  expect_s3_class(nabular(airquality), "data.frame")
  expect_s3_class(nabular(airquality), "tbl_df")
})

test_that("nabular errors when given non dataframe or 0 entry", {
  expect_snapshot(
    error = TRUE,
    nabular(0)
  )
  expect_snapshot(
    error = TRUE,
    nabular("a")
  )
  expect_snapshot(
    error = TRUE,
    nabular(matrix(0))
  )
  expect_snapshot(
    error = TRUE,
    nabular(NULL)
  )
})

test_that("The number of rows are the same after using bind_shadow", {
  expect_equal(nrow(nabular(airquality)), nrow(airquality))
})

test_that("The number of columns are twice the number of nabular", {
  expect_equal(ncol(nabular(airquality)), ncol(airquality) * 2)
})

test_that("nabular returns columns with additional suffix _NA", {
  expect_equal(
    names(nabular(airquality)),
    c(names(airquality), paste0(names(airquality), "_NA"))
  )
})

test_that("nabular with only_miss = TRUE returns smaller dataframe", {
  expect_gt(
    ncol(nabular(airquality)),
    ncol(nabular(airquality, only_miss = TRUE))
  )
})

test_that("nabular with only_miss = TRUE returns the right number of cols", {
  expect_equal(
    ncol(nabular(airquality, only_miss = TRUE)),
    ncol(airquality) + 2
  )
})

test_that("nabular with only_miss = TRUE returns columns with extra suffix _NA", {
  expect_equal(
    names(nabular(airquality, only_miss = TRUE)),
    c(names(airquality), paste0(miss_var_which(airquality), "_NA"))
  )
})
