context("test-impute-median")

vec <- rnorm(10)

vec[sample(1:10, 3)] <- NA

median_val <- median(vec, na.rm = TRUE)

vec2 <- vec

vec2[is.na(vec)] <- median_val

aq_shadow <- nabular(airquality)

test_that("impute_median works", {
  expect_equal(impute_median(vec), vec2)
})


## Scoped variants -------------------------------------------------------------
test_that("impute_median and scoped variants fail when given wrong input", {
  expect_error(impute_median_if(numeric(0)))
  expect_error(impute_median_at(numeric(0)))
  expect_error(impute_median_all(numeric(0)))

  expect_error(impute_median_if(NULL))
  expect_error(impute_median_at(NULL))
  expect_error(impute_mean_all(NULL))
})

## impute_median_if -------------------------------------------------------------

test_that("impute_median_if works", {
  expect_false(impute_median_if(airquality, is.numeric) %>% all_na())
})

test_that("impute_median_if works with shadow", {
  expect_false(impute_median_if(aq_shadow, is.numeric) %>% all_na())
})

test_that("impute_median_if retains proper shadow values", {
  expect_equal(unbind_data(impute_median_if(aq_shadow, is.numeric)),
               unbind_data(aq_shadow))
})

test_that("impute_median_if retains proper shadow values", {
  expect_equal(unbind_data(impute_median_if(aq_shadow, is.numeric)),
               unbind_data(aq_shadow))
})

## impute_median_at -------------------------------------------------------------
test_that("impute_median_at works", {
  expect_equal(impute_median_at(airquality,
                              vars(Ozone)) %>%
                 miss_var_which(),
               "Solar.R")
})

test_that("impute_median_at works with shadow", {
  expect_equal(impute_median_at(aq_shadow,
                              vars(Ozone)) %>%
                 miss_var_which(),
               "Solar.R")
})

test_that("impute_median_at retains proper shadow values", {
  expect_equal(unbind_data(impute_median_at(aq_shadow, vars(Ozone))),
               unbind_data(aq_shadow))
})

test_that("impute_median_at retains proper shadow values", {
  expect_equal(unbind_data(impute_median_at(aq_shadow, vars(Ozone))),
               unbind_data(aq_shadow))
})


## impute_median_all ------------------------------------------------------------
test_that("impute_median_all works", {
  expect_false(impute_median_all(airquality) %>% all_na())
})

test_that("impute_median_all works with shadow", {
  expect_false(impute_median_all(aq_shadow) %>% all_na())
})

test_that("impute_median_all retains proper shadow values", {
  skip_on_cran()
  expect_equal(unbind_data(impute_median_all(aq_shadow)),
               unbind_data(aq_shadow))
})

test_that("impute_median_all retains proper shadow values", {
  skip_on_cran()
  expect_equal(unbind_data(impute_median_all(aq_shadow)),
               unbind_data(aq_shadow))
})


test_that("impute_median_all works with shadow", {
  expect_false(impute_median_all(aq_shadow) %>% all_na())
})

test_that("impute_median_all retains proper shadow values", {
  skip_on_cran()
  expect_equal(unbind_data(impute_median_all(aq_shadow)),
               unbind_data(aq_shadow))
})

test_that("impute_median_all retains proper shadow values", {
  skip_on_cran()
  expect_equal(unbind_data(impute_median_all(aq_shadow)),
               unbind_data(aq_shadow))
})
