library(dplyr)
test_that("miss_var_cumsum gives warning", {
  expect_snapshot_warning(
    miss_var_cumsum(airquality)
  )
})

test_that("miss_var_cumsum gives warning with grouping", {
  expect_snapshot_warning(
    miss_var_cumsum(group_by(airquality, Month))
  )
})
