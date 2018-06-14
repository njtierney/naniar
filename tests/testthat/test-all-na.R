context("test all_na and all_complete")

misses <- c(NA, NA, NA)
complete <- c(1, 2, 3)
mixture <- c(NA, 1, NA)

test_that("all_na returns TRUE when all NA",{
  expect_true(all_na(misses))
})

test_that("all_complete returns FALSE when all missing",{
  expect_false(all_complete(misses))
})

test_that("all_complete returns TRUE when all complete",{
  expect_true(all_complete(complete))
})

test_that("all_na returns FALSE when all complete",{
  expect_false(all_na(complete))
})

test_that("all_na returns FALSE when mixture of missings",{
  expect_false(all_na(mixture))
})

test_that("all_complete returns FALSE when mixture of missings",{
  expect_false(all_na(mixture))
})
