test_that("shade errors with NULLs", {
  expect_snapshot(
    error = TRUE,
    shade(NULL)
  )
})

test_that("shade errors with objects of length 0", {
  expect_snapshot(
    error = TRUE,
    shade(numeric(0))
  )
})

test_that("shade errors with list of length 0", {
  expect_snapshot(
    error = TRUE,
    shade(list())
  )
})


test_that("shade returns an object of class shade", {
  expect_s3_class(shade(c(1, 2, NA)), "shade")
  expect_s3_class(shade(c(1, 2, NA), broken_machine = 2), "shade")
})

test_that("shade returns right levels when no extra levels provided", {
  expect_equal(levels(shade(c(1, 2, NA))), c("!NA", "NA"))
})

test_that("shade returns the correct levels when extra levels provided", {
  expect_equal(
    levels(shade(c(1, 2, NA), broken_machine = 2)),
    c("!NA", "NA", "NA_broken_machine")
  )
})

sh_1 <- shade(c(3, 1, 2, NA), broken = 3)
act_1 <- paste0(sh_1)
exp_1 <- c("NA_broken", "!NA", "!NA", "NA")

sh_2 <- shade(c(3, 1, 2, NA), broken = 1)
act_2 <- paste0(sh_2)
exp_2 <- c("!NA", "NA_broken", "!NA", "NA")

sh_3 <- shade(c(3, 1, 2, NA), broken = 2)
act_3 <- paste0(sh_3)
exp_3 <- c("!NA", "!NA", "NA_broken", "NA")

test_that("shade returns the correct values", {
  expect_equal(act_1, exp_1)
  expect_equal(act_2, exp_2)
  expect_equal(act_3, exp_3)
})

sh_4 <- shade(list(3, list(1), c(2, 3), list()))
act_4 <- paste0(sh_4)
exp_4 <- c("!NA", "!NA", "!NA", "NA")

test_that("shade returns the correct values with list columns", {
  expect_equal(act_4, exp_4)
  expect_snapshot(
    error = TRUE,
    shade(list(3, list(1), c(2, 3), list()), broken = 3)
  )
})
