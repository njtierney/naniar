context("miss-var-span")

library(dplyr)

p_group <- pedestrian %>% group_by(month)

test_that("miss_var_span doesn't return a warning", {
  expect_silent(miss_var_span(data = pedestrian,
                              var = hourly_counts,
                              span_every = 168))
  expect_silent(miss_var_span(data = p_group,
                              var = hourly_counts,
                              span_every = 168))
})

mvs <- miss_var_span(data = pedestrian,
                     var = hourly_counts,
                     span_every = 168)

mvs_g <- miss_var_span(data = p_group,
                       var = hourly_counts,
                       span_every = 168)

test_that("miss_var_span is a tbl", {
  expect_equal(class(mvs),c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(mvs_g),c("grouped_df", "tbl_df", "tbl", "data.frame"))
})

test_that("miss_var_span returns right dimensions",{
  expect_equal(ncol(mvs), 5)
  expect_equal(nrow(mvs), 225)
  expect_equal(ncol(mvs_g), 6)
  expect_equal(nrow(mvs_g), 230)
})


