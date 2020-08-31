context("as_shadow")

test_that("as_shadow returns a data.frame or tibble",{
  expect_is(as_shadow(airquality), "data.frame")
  expect_is(as_shadow(airquality), "tbl")
})

test_df <- data.frame(x = c(1,2),
                      y = c(NA,NA),
                      z = c(1, NA)) %>% as_shadow()

test_that("as_shadow returns correct values",{
  expect_equal(as.character(test_df$x_NA), c("!NA", "!NA"))
  expect_equal(as.character(test_df$y_NA), c("NA", "NA"))
  expect_equal(as.character(test_df$z_NA), c("!NA", "NA"))
})

test_that("as_shadow returns factors",{
  expect_is(test_df$x_NA, "factor")
  expect_is(test_df$y_NA, "factor")
  expect_is(test_df$z_NA, "factor")
})

test_that("as_shadow returns shade",{
  expect_is(test_df$x_NA, "shade")
  expect_is(test_df$y_NA, "shade")
  expect_is(test_df$z_NA, "shade")
})

test_that("as_shadow returns correct levels",{
  expect_equal(levels(test_df$x_NA), c("!NA", "NA"))
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
  expect_equal(sum(first_classes == "shade"), ncol(airquality))
})

test_that("as_shadow returns factor second",{
  expect_equal(sum(second_classes == "factor"), ncol(airquality))
})

test_that("as_shadow returns columns with additional suffix _NA",{
  expect_equal(names(as_shadow(airquality)), paste0(names(airquality),"_NA"))
})

