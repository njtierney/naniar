context("test-impute-median")

vec <- rnorm(10)

vec[sample(1:10, 3)] <- NA

median_val <- median(vec, na.rm = TRUE)

vec2 <- vec

vec2[is.na(vec)] <- median_val

test_that("impute_median works", {
  expect_equal(impute_median(vec),
               vec2)
})

test_that("impute_median_all works", {
  expect_false(impute_median_all(airquality) %>% all_na)
})

aq_shadow <- bind_shadow(airquality)

test_that("impute_median_all works with shadow", {
  expect_false(impute_median_all(aq_shadow) %>% all_na())
})

test_that("impute_median_all retains proper shadow values", {
  expect_equal(unbind_data(impute_median_all(aq_shadow)),
               unbind_data(aq_shadow))
})

test_that("impute_median_all retains proper shadow values", {
  expect_equal(unbind_data(impute_median_all(aq_shadow)),
               unbind_data(aq_shadow))
})
