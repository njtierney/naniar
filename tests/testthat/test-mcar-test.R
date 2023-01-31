test_that("mcar_test calculates correct statistics", {
  out_test <- mcar_test(airquality)

  expect_equal(out_test[["statistic"]], 35.1061288689702)
  expect_equal(out_test[["df"]], 14)
  expect_equal(out_test[["p.value"]], 0.00141778113856683)
  expect_equal(out_test[["missing.patterns"]], 4L)
})

test_that("mcar_test works when one of the missing patterns is all NAs", {
  airquality_all_missing <- dplyr::add_row(airquality, Ozone = NA)

  out_test <- mcar_test(airquality_all_missing)

  expect_equal(out_test[["statistic"]], 35.1061229683641)
  expect_equal(out_test[["missing.patterns"]], 5L)
})
