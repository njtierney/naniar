context("test-shade.R")

test_that("shade errors with NULLs",{
  expect_error(shade(NULL))
})

test_that("shade errors with objects of length 0",{
  expect_error(shade(numeric(0)))
})

test_that("shade errors with list of length 0",{
  expect_error(shade(list()))
})


test_that("shade returns an object of class shade", {
  expect_is(shade(c(1,2, NA)), "shade")
  expect_is(shade(c(1,2, NA),
                  broken_machine = 2), "shade")
})

test_that("shade returns right levels when no extra levels provided", {
  expect_equal(levels(shade(c(1,2, NA))),
               c("!NA", "NA"))
})

test_that("shade returns the correct levels when extra levels provided",{
  expect_equal(levels(shade(c(1,2, NA),
                            broken_machine = 2)),
               c("!NA", "NA", "NA_broken_machine"))
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

sh_4 <- shade(c(list(3), list(1), list(2,3), list()), broken = 2)
act_4 <- paste0(sh_4)
exp_4 <- c("!NA", "!NA", "NA_broken", "NA")

test_that("shade returns the correct values", {
  expect_equal(act_1, exp_1)
  expect_equal(act_2, exp_2)
  expect_equal(act_3, exp_3)
  expect_equal(act_4, exp_4)
})

