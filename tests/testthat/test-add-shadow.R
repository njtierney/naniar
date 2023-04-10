df <- data.frame(x = c(NA, 1:4),
             y = c(NA, NA, 1:3))

test_that("add_shadow returns a tibble",{
  expect_s3_class(add_shadow(df, x), "tbl_df")
})

test_that(
  "add_shadow returns a nice error message when no variables are provided",{
  expect_error(add_shadow(df))
})

test_that("add_shadow adds the right number of columns",{
  expect_equal(ncol(df)+1, ncol(add_shadow(df, x)))
})

test_that("add_shadow adds a column with suffix '_NA'",{
  expect_equal(names(add_shadow(df, x)),
               c(names(df), "x_NA"))
})
