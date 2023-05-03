vec <- rnorm(10)

vec[sample(1:10, 3)] <- NA

fixed_val <- -99

vec2 <- vec

vec2[is.na(vec)] <- fixed_val

aq_shadow <- nabular(airquality)

test_that("replace_na_with works", {
  expect_equal(replace_na_with(vec, -99), vec2)
})

## replace_na_with_across --------------------------------------------------------
library(dplyr)
test_that("replace_na_with works with across", {
  expect_false(
    mutate(airquality,
           across(where(is.numeric), \(x) replace_na_with(x, -99))) %>%
      all_na()
  )
})

test_that("replace_na_with works with across and nabular", {
  expect_false(
    mutate(aq_shadow,
           across(where(is.numeric), \(x) replace_na_with(x, -99))) %>%
      all_na()
  )
})

test_that("replace_na_with retains proper shadow values when used with across", {
  expect_equal(
    unbind_data(
      mutate(aq_shadow,
             across(where(is.numeric), \(x) replace_na_with(x, -99)))
    ),
    unbind_data(
      aq_shadow
    )
  )
})
