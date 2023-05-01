miss_vec_5 <- c(10,10,9,NA,3)
miss_vec_2 <- c(4,NA)
miss_vec_3 <- c(4,NA,NA)
miss_vec_4 <- c(4,NA,NA,NA)

test_that("shadow_shift returns soft deprecation warning", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_snapshot(
    shadow_shift(NULL)
  )
  expect_snapshot(
    error = TRUE,
    shadow_shift(3i)
  )
  expect_snapshot_warning(
    shadow_shift(miss_vec_5)
  )
})

test_that("shadow_shift still works", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_snapshot(
    shadow_shift(NULL)
  )
  expect_snapshot(
    error = TRUE,
    shadow_shift(3i)
  )
  expect_snapshot(
    shadow_shift(miss_vec_5)
  )
})
