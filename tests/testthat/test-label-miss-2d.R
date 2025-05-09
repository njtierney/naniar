test_df <- data.frame(x = c(NA, 2, 3), y = c(1, NA, 3), z = c(1, 2, 3))

test_that("label_miss_2d errors on the first NULL entry", {
  expect_snapshot(
    error = TRUE,
    label_miss_2d(NULL, 3)
  )
})

test_that("label_miss_2d errors on the second NULL entry", {
  expect_snapshot(
    error = TRUE,
    label_miss_2d(3, NULL)
  )
})

test_that("label_miss_2d errors when both are NULL", {
  expect_snapshot(
    error = TRUE,
    label_miss_2d(NULL, NULL)
  )
})

test_that("label_miss_2d returns a vector of the same length as the input", {
  expect_length(
    label_miss_2d(test_df$x, test_df$y),
    nrow(test_df)
  )
})

test_that("label_miss_2d returns factor vector", {
  expect_type(
    label_miss_2d(test_df$x, test_df$y),
    # typeof(factor()) is "integer"
    "integer"
  )
})


test_that("label_miss_2d identifies the correct location of missingness", {
  expect_snapshot(
    label_miss_2d(test_df$x, test_df$y)
  )
  expect_snapshot(
    label_miss_2d(test_df$y, test_df$z)
  )
  expect_snapshot(
    label_miss_2d(test_df$x, test_df$z)
  )
})
