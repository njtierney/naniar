miss_vec_5 <- c(10,10,9,NA,3)
miss_vec_2 <- c(4,NA)
miss_vec_3 <- c(4,NA,NA)
miss_vec_4 <- c(4,NA,NA,NA)

test_that("shadow_shift returns soft deprecation warning", {
  expect_snapshot(
    shadow_shift(NULL)
  )
  expect_snapshot(
    error = TRUE,
    shadow_shift(as.POSIXct(111, origin = "1970-01-01"))
  )
  expect_snapshot(
    shadow_shift(miss_vec_5)
  )
})
