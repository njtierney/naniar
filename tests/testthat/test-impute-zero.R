vec <- rnorm(10)

vec[sample(1:10, 3)] <- NA

vec2 <- vec

vec2[is.na(vec)] <- 0

aq_shadow <- nabular(airquality)

test_that("impute_zero works", {
  expect_equal(impute_zero(vec), vec2)
})

## impute_zero_across --------------------------------------------------------
library(dplyr)
test_that("impute_zero works with across", {
  expect_false(
    mutate(airquality,
           across(where(is.numeric), \(x) impute_zero(x))) %>%
      all_na()
  )
})

test_that("impute_zero works with across and nabular", {
  expect_false(
    mutate(aq_shadow,
           across(where(is.numeric), \(x) impute_zero(x))) %>%
      all_na()
  )
})

test_that("impute_zero retains proper shadow values when used with across", {
  expect_equal(
    unbind_data(
      mutate(aq_shadow,
             across(where(is.numeric), \(x) impute_zero(x)))
    ),
    unbind_data(
      aq_shadow
    )
  )
})
