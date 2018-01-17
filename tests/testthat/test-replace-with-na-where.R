context("Test replace_with_na_where")

df <- tibble::tribble(~x,  ~y,    ~z,
                      1L,   "A",   -100,
                      3L,   "N/A", -99,
                      NA,   NA,    -98,
                      -99L, "E",   -101,
                      -98L, "F",   -1)


# test_that("check if it can handle using an anonymous function", {
#   is_zero <- function(x) x == 0
#   expect_success(replace_with_na_where(mtcars, vs = is_zero))
#   expect_success(replace_with_na_where(mtcars, vs = vs == 0))
# })

test_that("Values are affected by _where in the same column",{
  out <- replace_with_na_where(data = df,
                               x = x == -99)
  expect_equal(out$x, c(1,3,NA,NA,-98))
  expect_equal(out$z, c(-100, -99, -98, -101, -1))
})

test_that("Check specifying conditions from other columns works",{
  out <- replace_with_na_where(data = df,
                               y = x == -99)
  expect_equal(out$x, c(1,3,NA,-99,-98))
  expect_equal(out$y, c("A", "N/A", NA, NA, "F"))
  expect_equal(out$z, c(-100, -99, -98, -101, -1))
})

test_that("Check specifying conditions from all columns works",{
  out <- replace_with_na_where(data = df,
                               x = y == "A",
                               y = x == 1,
                               z = x == -98)
  expect_equal(out$x, c(NA,3,NA,-99,-98))
  expect_equal(out$y, c(NA, "N/A", NA, "E", "F"))
  expect_equal(out$z, c(-100, -99, -98, -101, NA))
})

test_that("Check specifying conditions in a different order works",{
  out <- replace_with_na_where(data = df,
                               y = x == 1,
                               x = y == "A",
                               z = x == -98)
  expect_equal(out$y, c(NA, "N/A", NA, "E", "F"))
  expect_equal(out$x, c(NA,3,NA,-99,-98))
  expect_equal(out$z, c(-100, -99, -98, -101, NA))
})
