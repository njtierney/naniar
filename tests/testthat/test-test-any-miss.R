misses <- c(NA, NA, NA)
complete <- c(1, 2, 3)
mixture <- c(NA, 1, NA)

test_that("any_na and any_miss returns TRUE when there are all NAs",{
  expect_true(any_na(misses))
  expect_true(any_miss(misses))
})

test_that("any_na and any_miss returns TRUE when there are some NAs",{
  expect_true(any_na(mixture))
  expect_true(any_miss(mixture))
})

test_that("any_na and any_miss returns FALSE when there only complete values",{
  expect_false(any_na(complete))
  expect_false(any_miss(complete))
})

test_that("any_complete returns FALSE when all missing",{
  expect_false(any_complete(misses))
})

test_that("any_complete returns TRUE when some missing",{
  expect_true(any_complete(mixture))
})

test_that("any_complete returns TRUE when only complete",{
  expect_true(any_complete(complete))
})
