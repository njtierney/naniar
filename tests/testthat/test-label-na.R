context("label_na behaves")

test_that("label_na returns !NA and NA for cases we expect",{
  expect_equal(paste(label_na(c(1,2,3))),c("!NA","!NA","!NA"))
  expect_equal(paste(label_na(c(1,NA,3))),c("!NA","NA","!NA"))
})

test_that("is_na returns the levels !NA and NA",{
  expect_equal(levels(label_na(1)),c("!NA","NA"))
})

test_that("is_na errors with length 0 objects",{
  expect_error(label_na(numeric(0)))
})
