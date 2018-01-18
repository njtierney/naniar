context("replace_to_na deprecation test")


test_that("replace_to_na is deprecated", {
  df <- tibble::tibble(x = c("A", NA))
  expect_warning(replace_to_na(df),
                 "'replace_to_na' is deprecated")
})

context("replace_with_na")

test_that("empty call does nothing", {
  df <- tibble::tibble(x = c("A", NA))
  out <- replace_with_na(df)
  expect_equal(out, df)
})

test_that("suggested single values are replaced with NAs", {
  df <- tibble::tibble(x = c(1, NA, -99))
  out <- replace_with_na(df, list(x = -99))
  expect_equal(out$x, c(1, NA, NA))
})

test_that("suggested multiple values are replaced with NAs", {
  df <- tibble::tibble(x = c(1, NA, -99, -98))
  out <- replace_with_na(df, list(x = c(-98, -99)))
  expect_equal(out$x, c(1, NA, NA, NA))
})

dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                         1,   "A",   -100,
                         3,   "N/A", -99,
                         NA,  NA,    -98,
                         -99, "E",   -101,
                         -98, "F",   -1)

test_that("only specified columns are affected",{
  df <- dat_ms
  out <- replace_with_na(df, list(x = c(-99)))
  expect_equal(out$x, c(1,3,NA,NA,-98))
  expect_equal(out$z, c(-100, -99, -98, -101, -1))
})

test_that("works for multiple columns",{
  df <- dat_ms
  out <- replace_with_na(df, list(x = c(-99, -98),
                                y = c("N/A"),
                                z = c(-1, -99)))
  expect_equal(out$x, c(1,3,NA,NA,NA))
  expect_equal(out$y, c("A",NA,NA,"E","F"))
  expect_equal(out$z, c(-100, NA, -98, -101, NA))
})
