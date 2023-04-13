vec <- rnorm(10)

vec[sample(1:10, 3)] <- NA

fixed_val <- -99

vec2 <- vec

vec2[is.na(vec)] <- fixed_val

aq_shadow <- nabular(airquality)

test_that("impute_fixed works", {
  expect_equal(impute_fixed(vec, -99), vec2)
})

## impute_fixed_across --------------------------------------------------------
library(dplyr)
test_that("impute_fixed works with across", {
  expect_false(
    mutate(airquality,
           across(where(is.numeric), \(x) impute_fixed(x, -99))) %>%
      all_na()
  )
})

test_that("impute_fixed works with across and nabular", {
  expect_false(
    mutate(aq_shadow,
           across(where(is.numeric), \(x) impute_fixed(x, -99))) %>%
      all_na()
    )
})

test_that("impute_fixed retains proper shadow values when used with across", {
  expect_equal(
    unbind_data(
      mutate(aq_shadow,
             across(where(is.numeric), \(x) impute_fixed(x, -99)))
      ),
    unbind_data(
      aq_shadow
      )
    )
})
