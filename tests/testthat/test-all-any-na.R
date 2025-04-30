misses <- c(NA, NA, NA)
complete <- c(1, 2, 3)
mixture <- c(NA, 1, NA)

df_misses <- data.frame(x = misses, y = misses)
df_complete <- data.frame(x = complete, y = complete)
df_mixture <- data.frame(x = mixture, y = mixture)
df_mixture2 <- data.frame(x = complete, y = misses)

any_not_na <- Negate(any_na)

test_that("all_na returns correct response", {
  expect_true(all_na(misses))
  expect_false(all_na(complete))
  expect_false(all_na(mixture))
  expect_true(all_na(df_misses))
  expect_false(all_na(df_complete))
  expect_false(all_na(df_mixture))
  expect_false(all_na(df_mixture2))
})

test_that("all_complete returns correct response", {
  expect_false(all_complete(misses))
  expect_true(all_complete(complete))
  expect_false(all_complete(mixture))
  expect_false(all_complete(df_misses))
  expect_true(all_complete(df_complete))
  expect_false(all_complete(df_mixture))
  expect_false(all_complete(df_mixture2))
})

test_that("any_na returns correct response", {
  expect_true(any_na(misses))
  expect_false(any_na(complete))
  expect_true(any_na(mixture))
  expect_true(any_na(df_misses))
  expect_false(any_na(df_complete))
  expect_true(any_na(df_mixture))
  expect_true(any_na(df_mixture2))
})

test_that("any_complete returns correct response", {
  expect_false(any_complete(misses))
  expect_true(any_complete(complete))
  expect_true(any_complete(mixture))
  expect_false(any_complete(df_misses))
  expect_true(any_complete(df_complete))
  expect_true(any_complete(df_mixture))
  expect_false(any_complete(df_mixture2))
})
