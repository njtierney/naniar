x <- c(NA, 1, 2, "3")
xs <- shade(c(NA, 1, 2, "3"))
aq <- airquality
aq_s <- as_shadow(airquality)

test_that("is_shade returns appropriate output", {
  expect_true(is_shade(xs))
  expect_false(is_shade(x))
  expect_false(is_shade(aq_s))
  expect_false(is_shade(aq))
})

test_that("are_shade returns appropriate output", {
  expect_true(all(are_shade(xs)))
  expect_false(all(are_shade(x)))
  expect_true(all(are_shade(aq_s)))
  expect_false(all(are_shade(aq)))
})

test_that("any_shade returns appropriate output", {
  expect_true(any_shade(xs))
  expect_false(any_shade(x))
  expect_true(any_shade(aq_s))
  expect_false(any_shade(aq))
})
