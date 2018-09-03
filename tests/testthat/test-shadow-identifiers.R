context("test-shadow-identifiers")

df <- tibble::tribble(
  ~wind, ~temp,
  -99,    45,
  68,    NA,
  72,    25
)

dfs <- bind_shadow(df)

test_that("is_nabular returns TRUE for nabular data", {
  expect_true(is_nabular(dfs))
})

test_that("is_shadow returns FALSE for regular data", {
  expect_false(is_shadow(df))
})

dfs_are <- are_shade(dfs)

test_that("are_shade returns the correct values",{
  expect_false(dfs_are[["wind"]])
  expect_false(dfs_are[["temp"]])
  expect_true(dfs_are[["wind_NA"]])
expect_true(dfs_are[["temp_NA"]])
})

test_that("any_shade returns the correct values",{
  expect_false(any_shade(df))
  expect_true(any_shade(dfs))
})

