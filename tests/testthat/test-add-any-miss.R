df <- data.frame(x = c(NA, 1:4), y = c(NA, NA, 1:3))

test_that("add_any_miss returns a tibble", {
  expect_s3_class(add_any_miss(df, x), "tbl_df")
})

test_that("add_any_miss respects dimensions", {
  expect_equal(ncol(df) + 1, ncol(add_any_miss(df, x, y)))
  expect_equal(nrow(df), nrow(add_any_miss(df, x, y)))
})

test_that("add_any_miss adds a column with suffix 'any_miss_vars' or 'any_miss_all'", {
  expect_equal(names(add_any_miss(df, x, y)), c(names(df), "any_miss_vars"))
  expect_equal(names(add_any_miss(df)), c(names(df), "any_miss_all"))
})
