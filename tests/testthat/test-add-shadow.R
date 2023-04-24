dat <- data.frame(x = c(NA, 1:4),
             y = c(NA, NA, 1:3))

test_that("add_shadow returns a tibble",{
  expect_s3_class(add_shadow(dat, x), "tbl_df")
})

test_that(
  "add_shadow returns a nice error message when no variables are provided",{
  expect_snapshot(
    error = TRUE,
    add_shadow(dat)
    )
})

test_that("add_shadow adds the right number of columns",{
  expect_equal(ncol(dat)+1, ncol(add_shadow(dat, x)))
})

test_that("add_shadow adds a column with suffix '_NA'",{
  expect_equal(names(add_shadow(dat, x)),
               c(names(dat), "x_NA"))
})
