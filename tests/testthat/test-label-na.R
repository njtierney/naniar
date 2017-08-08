context("label_shadow_matrix behaves")

test_that("label_shadow_matrix returns !NA and NA for cases we expect",{
  expect_equal(paste(label_shadow_matrix(c(1,2,3))),c("!NA","!NA","!NA"))
  expect_equal(paste(label_shadow_matrix(c(1,NA,3))),c("!NA","NA","!NA"))
})

test_that("is_na returns the levels !NA and NA",{
  expect_equal(levels(label_shadow_matrix(1)),c("!NA","NA"))
})

test_that("is_na errors with length 0 objects",{
  expect_error(label_shadow_matrix(numeric(0)))
})
