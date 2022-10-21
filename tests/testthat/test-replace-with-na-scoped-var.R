test_that("empty call provides an error", {
  df <- tibble::tibble(x = c("A", NA))
  expect_error(replace_with_na_all(df))
})

test_that("suggested single values are replaced with NAs", {
  df <- tibble::tibble(x = c(1, NA, -99))
  out <- replace_with_na_all(df, ~.x == -99)
  expect_equal(out$x, c(1, NA, NA))
})

test_that("suggested multiple values are replaced with NAs", {
  df <- tibble::tibble(x = c(1, NA, -99, -98))
  out <- replace_with_na_all(df, ~.x %in% c(-98, -99))
  expect_equal(out$x, c(1, NA, NA, NA))
})

dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                          1,   "A",   -100,
                          3,   "N/A", -99,
                          NA,  NA,    -98,
                          -99, "E",   -101,
                          -98, "F",   -1)

test_that("all columns are affected by _all",{
  df <- dat_ms
  out <- replace_with_na_all(df, ~.x == -99)
  expect_equal(out$x, c(1,3,NA,NA,-98))
  expect_equal(out$z, c(-100, NA, -98, -101, -1))
})

test_that("1 column selection affected by _at",{
  df <- dat_ms
  out <- replace_with_na_at(data = df,
                          .vars = "x",
                          condition = ~.x == -99)
  expect_equal(out$x, c(1,3,NA,NA,-98))
  expect_equal(out$z, c(-100, -99, -98, -101, -1))
})

test_that("2 column selection affected by _at",{
  df <- dat_ms
  out <- replace_with_na_at(data = df,
                          .vars = c("x","z"),
                          condition = ~.x == -99)
  expect_equal(out$x, c(1,3,NA,NA,-98))
  expect_equal(out$z, c(-100, NA, -98, -101, -1))
})

dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                          1L,   "A",   -100,
                          3L,   "N/A", -99,
                          NA,  NA,    -98,
                          -99L, "E",   -101,
                          -98L, "F",   -1)


test_that("columns are affected by _if",{
  df <- dat_ms
  out <- replace_with_na_if(data = df,
                          .predicate = is.integer,
                          condition = ~.x == -99)
  expect_equal(out$x, c(1,3,NA,NA,-98))
  expect_equal(out$z, c(-100, -99, -98, -101, -1))
})
