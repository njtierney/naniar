context("impute_mean")

vec <- rnorm(10)

vec[sample(1:10, 3)] <- NA

mean_val <- mean(vec, na.rm = TRUE)

vec2 <- vec

vec2[is.na(vec)] <- mean_val

test_that("impute_mean works", {
  expect_equal(impute_mean(vec),
               vec2)
})

test_that("impute_mean_all works", {
  expect_false(impute_mean_all(airquality) %>% all_na)
})

aq_shadow <- bind_shadow(airquality)

test_that("impute_mean_all works with shadow", {
  expect_false(impute_mean_all(aq_shadow) %>% all_na)
})

test_that("impute_mean_all retains proper shadow values", {
  expect_equal(unbind_data(impute_mean_all(aq_shadow)),
               unbind_data(aq_shadow))
})

test_that("impute_mean_all retains proper shadow values", {
  expect_equal(unbind_data(impute_mean_all(aq_shadow)),
               unbind_data(aq_shadow))
})
