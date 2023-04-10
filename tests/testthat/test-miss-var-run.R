library(dplyr)

p_group <- pedestrian %>% group_by(month)

test_that("miss_var_run doesn't return a warning", {
  expect_silent(miss_var_run(data = pedestrian,
                             var = hourly_counts))
  expect_silent(miss_var_run(data = p_group,
                             var = hourly_counts))
})

mvr <- miss_var_run(data = pedestrian,
                     var = hourly_counts)

mvr_g <- miss_var_run(data = p_group,
                       var = hourly_counts)

test_that("miss_var_run is a tbl", {
  expect_equal(class(mvr),c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(mvr_g),c("grouped_df", "tbl_df", "tbl", "data.frame"))
})

test_that("miss_var_run returns right dimensions", {
  expect_equal(ncol(mvr), 2)
  expect_equal(nrow(mvr), 35)
  expect_equal(ncol(mvr_g), 3)
  expect_equal(nrow(mvr_g), 51)
})
