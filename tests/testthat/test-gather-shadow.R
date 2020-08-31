context("gather_shadow")

df <- data.frame(x = c(NA, 1:4),
             y = c(NA, NA, 1:3))

test_that("gather_shadow returns a tibble or data.frame",{
  expect_is(gather_shadow(df), "data.frame")
  expect_is(gather_shadow(df), "tbl")
})

test_that("gather_shadow errors when given non dataframe or 0 entry",{
  expect_error(gather_shadow(0))
  expect_error(gather_shadow(matrix(0)))
  expect_error(gather_shadow(NULL))
})

test_that("The number of rows are the same after using gather_shadow",{
  expect_equal(nrow(gather_shadow(df)),nrow(df)*2)
})

test_that("There are three columns in gather_shadow",{
  expect_equal(ncol(gather_shadow(df)),3)
})

test_that("gather_shadow returns columns with right names",{
  expect_equal(names(gather_shadow(df)),
               c("case","variable","missing"))
})
