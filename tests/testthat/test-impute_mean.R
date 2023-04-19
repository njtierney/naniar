vec <- rnorm(10)

vec[sample(1:10, 3)] <- NA

mean_val <- mean(vec, na.rm = TRUE)

vec2 <- vec

vec2[is.na(vec)] <- mean_val

aq_shadow <- nabular(airquality)


test_that("impute_mean and scoped variants fail when given wrong input", {
  expect_snapshot(
    error = TRUE,
    impute_mean_if(numeric(0))
    )
  expect_snapshot(
    error = TRUE,
    impute_mean_at(numeric(0))
    )
  expect_snapshot(
    error = TRUE,
    impute_mean_all(numeric(0))
    )
  expect_snapshot(
    error = TRUE,
    impute_mean_if(NULL)
    )
  expect_snapshot(
    error = TRUE,
    impute_mean_at(NULL)
    )
  expect_snapshot(
    error = TRUE,
    impute_mean_all(NULL)
    )
})


test_that("impute_mean works", {
  expect_equal(impute_mean(vec), vec2)
})

### Scoped variants
test_that("impute_mean and scoped variants fail when given wrong input", {
  expect_snapshot(
    error = TRUE,
    impute_mean_if(numeric(0))
    )
  expect_snapshot(
    error = TRUE,
    impute_mean_at(numeric(0))
    )
  expect_snapshot(
    error = TRUE,
    impute_mean_all(numeric(0))
    )
  expect_snapshot(
    error = TRUE,
    impute_mean_if(NULL)
    )
  expect_snapshot(
    error = TRUE,
    impute_mean_at(NULL)
    )
  expect_snapshot(
    error = TRUE,
    impute_mean_all(NULL)
    )
})

## impute_mean_if -------------------------------------------------------------

test_that("impute_mean_if works", {
  expect_false(impute_mean_if(airquality, is.numeric) %>% all_na())
})

test_that("impute_mean_if works with shadow", {
  expect_false(impute_mean_if(aq_shadow, is.numeric) %>% all_na())
})

test_that("impute_mean_if retains proper shadow values", {
  expect_equal(unbind_data(impute_mean_if(aq_shadow, is.numeric)),
               unbind_data(aq_shadow))
})

test_that("impute_mean_if retains proper shadow values", {
  expect_equal(unbind_data(impute_mean_if(aq_shadow, is.numeric)),
               unbind_data(aq_shadow))
})

## impute_mean_at -------------------------------------------------------------
test_that("impute_mean_at works", {
  expect_equal(impute_mean_at(airquality,
                              vars(Ozone)) %>%
                 miss_var_which(),
               "Solar.R")
})

test_that("impute_mean_at works with shadow", {
  expect_equal(impute_mean_at(aq_shadow,
                              vars(Ozone)) %>%
                 miss_var_which(),
               "Solar.R")
})

test_that("impute_mean_at retains proper shadow values", {
  skip_on_cran()
  expect_equal(unbind_data(impute_mean_at(aq_shadow, vars(Ozone))),
               unbind_data(aq_shadow))
})

test_that("impute_mean_at retains proper shadow values", {
  skip_on_cran()
  expect_equal(unbind_data(impute_mean_at(aq_shadow, vars(Ozone))),
               unbind_data(aq_shadow))
})


## impute_mean_all ------------------------------------------------------------
test_that("impute_mean_all works", {
  expect_false(impute_mean_all(airquality) %>% all_na())
})

test_that("impute_mean_all works with shadow", {
  expect_false(impute_mean_all(aq_shadow) %>% all_na())
})

test_that("impute_mean_all retains proper shadow values", {
  skip_on_cran()
  expect_equal(unbind_data(impute_mean_all(aq_shadow)),
               unbind_data(aq_shadow))
})

test_that("impute_mean_all retains proper shadow values", {
  skip_on_cran()
  expect_equal(unbind_data(impute_mean_all(aq_shadow)),
               unbind_data(aq_shadow))
})
