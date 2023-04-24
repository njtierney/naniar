test_that("prop_miss* errors on NULL", {
  expect_snapshot(error = TRUE, prop_miss(NULL))
  expect_snapshot(error = TRUE, prop_miss_var(NULL))
  expect_snapshot(error = TRUE, prop_miss_case(NULL))
})

test_that("pct_miss* errors on NULL", {
  expect_snapshot(error = TRUE, pct_miss(NULL))
  expect_snapshot(error = TRUE, pct_miss_var(NULL))
  expect_snapshot(error = TRUE, pct_miss_case(NULL))
})

test_that("prop_complete* errors on NULL", {
  expect_snapshot(error = TRUE, prop_complete(NULL))
  expect_snapshot(error = TRUE, prop_complete_var(NULL))
  expect_snapshot(error = TRUE, prop_complete_case(NULL))
})

test_that("pct_complete* errors on NULL", {
  expect_snapshot(error = TRUE, pct_complete(NULL))
  expect_snapshot(error = TRUE, pct_complete_var(NULL))
  expect_snapshot(error = TRUE, pct_complete_case(NULL))
})

test_that("prop_miss* errors when a non-dataframe given", {
  expect_snapshot(error = TRUE, prop_miss_var(1))
  expect_snapshot(error = TRUE, prop_miss_var(matrix(0)))

  expect_snapshot(error = TRUE, prop_miss_case(1))
  expect_snapshot(error = TRUE, prop_miss_case(matrix(0)))
})

test_that("prop_complete* errors when a non-dataframe given", {
  expect_snapshot(error = TRUE, prop_complete_var(1))
  expect_snapshot(error = TRUE, prop_complete_var(matrix(0)))

  expect_snapshot(error = TRUE, prop_complete_case(1))
  expect_snapshot(error = TRUE, prop_complete_case(matrix(0)))
})
test_that("pct_miss* errors when a non-dataframe given", {
  expect_snapshot(error = TRUE, pct_miss_var(1))
  expect_snapshot(error = TRUE, pct_miss_var(matrix(0)))

  expect_snapshot(error = TRUE, pct_miss_case(1))
  expect_snapshot(error = TRUE, pct_miss_case(matrix(0)))
})

test_that("pct_complete* errors when a non-dataframe given", {
  expect_snapshot(error = TRUE, pct_complete_var(1))
  expect_snapshot(error = TRUE, pct_complete_var(matrix(0)))

  expect_snapshot(error = TRUE, pct_complete_case(1))
  expect_snapshot(error = TRUE, pct_complete_case(matrix(0)))
})

df <- data.frame(
  x = c(NA, 1:4),
  y = c(NA, NA, 1:3)
)

test_that("missingness scalar summaries produce a single, numeric number", {
  expect_length(prop_miss(df), 1)
  expect_type(prop_miss(df), "double")

  expect_length(pct_miss(df), 1)
  expect_type(pct_miss(df), "double")

  expect_length(prop_miss_var(df), 1)
  expect_type(prop_miss_var(df), "double")

  expect_length(pct_miss_var(df), 1)
  expect_type(pct_miss_var(df), "double")

  expect_length(prop_miss_case(df), 1)
  expect_type(prop_miss_case(df), "double")

  expect_length(pct_miss_case(df), 1)
  expect_type(pct_miss_case(df), "double")

  expect_length(prop_complete(df), 1)
  expect_type(prop_complete(df), "double")

  expect_length(pct_complete(df), 1)
  expect_type(pct_complete(df), "double")

  expect_length(prop_complete_var(df), 1)
  expect_type(prop_complete_var(df), "double")

  expect_length(pct_complete_var(df), 1)
  expect_type(pct_complete_var(df), "double")

  expect_length(prop_complete_case(df), 1)
  expect_type(prop_complete_case(df), "double")

  expect_length(pct_complete_case(df), 1)
  expect_type(pct_complete_case(df), "double")
})
