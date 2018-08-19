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
  expect_false(dfs_are[1])
  expect_false(dfs_are[2])
  expect_true(dfs_are[3])
  expect_true(dfs_are[3])
})

test_that("any_shadow returns the correct values",{
  expect_false(any_shadow(df))
  expect_true(any_shadow(dfs))
})

