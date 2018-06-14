context("as_shadow")

test_that("as_shadow returns a data.frame",{
  expect_s3_class(as_shadow(airquality),
            "data.frame")
})

test_that("as_shadow returns a tibble",{
  expect_equal(class(as_shadow(airquality)),
            c("shadow", "tbl_df", "tbl", "data.frame"))
})

test_that("as_shadow errors when given non dataframe or 0 entry",{
  expect_error(as_shadow(0))
  expect_error(as_shadow("a"))
  expect_error(as_shadow(matrix(airquality)))
  expect_error(as_shadow(NULL))
})

test_that("The dimensions are the same after using as_shadow",{
  expect_equal(dim(as_shadow(airquality)),dim(airquality))
})

library(purrr)

aq_shadow <- as_shadow(airquality)

classes <- map(aq_shadow,class)

first_classes <- map(classes, pluck, 1)
second_classes <- map(classes, pluck, 2)

test_that("as_shadow returns shadow first",{
  expect_equal(sum(first_classes == "shadow"),
               ncol(airquality))
})

test_that("as_shadow returns factor second",{
  expect_equal(sum(second_classes == "factor"),
               ncol(airquality))
})


test_that("as_shadow returns columns with additional suffix _NA",{
  expect_equal(names(as_shadow(airquality)),
               paste0(names(airquality),"_NA"))
})

