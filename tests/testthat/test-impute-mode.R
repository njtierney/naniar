vec <- rnorm(10)

vec[sample(1:10, 3)] <- NA

vec2 <- vec

vec2[is.na(vec)] <- the_mode(vec, na.rm = TRUE)

aq_shadow <- nabular(airquality)

test_that("impute_mode works", {
  expect_equal(impute_mode(vec), vec2)
})

## impute_mode_across --------------------------------------------------------
library(dplyr)
test_that("impute_mode works with across", {
  expect_false(
    mutate(airquality, across(where(is.numeric), \(x) impute_mode(x))) %>%
      all_na()
  )
})

test_that("impute_mode works with across and nabular", {
  expect_false(
    mutate(aq_shadow, across(where(is.numeric), \(x) impute_mode(x))) %>%
      all_na()
  )
})

test_that("impute_mode retains proper shadow values when used with across", {
  expect_equal(
    unbind_data(
      mutate(aq_shadow, across(where(is.numeric), \(x) impute_mode(x)))
    ),
    unbind_data(
      aq_shadow
    )
  )
})
