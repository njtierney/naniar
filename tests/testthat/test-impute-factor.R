library(dplyr)
vec <- factor(x = c(NA, LETTERS[1:4]))

vec2 <- factor(x = c("wat", LETTERS[1:4]))

chick <- chickwts %>%
  mutate(
    feed = set_prop_miss(feed, prop = 0.2)
  )
chick_shadow <- nabular(chick)

test_that("impute_factor works", {
  expect_equal(impute_factor(vec, "wat"), vec2)
})

## impute_factor_across --------------------------------------------------------
test_that("impute_factor works with across", {
  expect_false(
    mutate(chick,
           across(where(is.factor), \(x) impute_factor(x, "wat"))) %>%
      all_na()
  )
})

test_that("impute_factor works with across and nabular", {
  expect_false(
    mutate(chick_shadow,
           across(where(is.factor), \(x) impute_factor(x, "wat"))) %>%
      all_na()
  )
})

test_that("impute_factor retains proper shadow values when used with across", {
  expect_equal(
    unbind_data(
      mutate(chick_shadow,
             across(where(is.factor), \(x) impute_factor(x, "wat")))
    ),
    unbind_data(
      chick_shadow
    )
  )
})
