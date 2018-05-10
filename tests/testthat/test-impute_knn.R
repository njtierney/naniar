context("impute_knn")

test_that("impute_knn random works", {
  expect_false(impute_knn(airquality, method = "random") %>% all_na)
})

aq_shadow <- bind_shadow(airquality)

test_that("impute_knn random works with shadow", {
  expect_false(impute_knn(aq_shadow, method = "random") %>% all_na)
})

test_that("impute_knn mean works", {
  expect_false(impute_knn(aq_shadow, method = "mean") %>% all_na)
})
