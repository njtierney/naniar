context("is_na behaves")

test_that("is_na returns !NA and NA for cases we expect",{
  expect_equal(paste(is_na(c(1,2,3))),c("!NA","!NA","!NA"))
  expect_equal(paste(is_na(c(1,NA,3))),c("!NA","NA","!NA"))
})

test_that("is_na returns the levels !NA and NA",{
  expect_equal(levels(is_na(1)),c("!NA","NA"))
})

test_that("is_na errors with length 0 objects",{
  expect_error(is_na(numeric(0)))
})
