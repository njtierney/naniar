context("test-shadow-identifiers")

df <- tibble::tribble(
  ~wind, ~temperature,
  -99,    45,
  68,    NA,
  72,    25
)

dfs <- bind_shadow(df)

test_that("is_shadow returns TRUE for a shadow", {
  expect_true(is_shadow(dfs))
})

test_that("is_shadow returns FALSE for regular data", {
  expect_false(is_shadow(df))
})

dfs_are <- are_shadow(dfs)

test_that("are_shadow returns the correct values",{
  expect_false(dfs_are[["wind"]])
  expect_false(dfs_are[["temperature"]])
  expect_true(dfs_are[["wind_NA"]])
  expect_true(dfs_are[["temperature_NA"]])
})

test_that("any_shadow returns the correct values",{
  expect_false(any_shadow(df))
  expect_true(any_shadow(dfs))
})

