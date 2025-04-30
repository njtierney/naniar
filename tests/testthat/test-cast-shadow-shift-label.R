test_that("cast_shadow_shift_label returns nice error if variables aren't included", {
  expect_snapshot(
    error = TRUE,
    cast_shadow_shift(airquality)
  )
})

test_that("cast_shadow_shift_label returns a tibble", {
  expect_s3_class(cast_shadow_shift_label(airquality, Ozone), "tbl_df")
})

test_that("cast_shadow_shift_label adds the right number of columns", {
  expect_equal(4, ncol(cast_shadow_shift_label(airquality, Ozone)))
  expect_equal(7, ncol(cast_shadow_shift_label(airquality, Ozone, Solar.R)))
  expect_equal(
    10,
    ncol(cast_shadow_shift_label(airquality, Ozone, Solar.R, Temp))
  )
})

test_that("cast_shadow_shift_label adds a column with suffix '_NA' AND '_shift'", {
  expect_equal(
    names(cast_shadow_shift_label(airquality, Ozone)),
    c("Ozone", "Ozone_NA", "Ozone_shift", "any_missing")
  )
  expect_equal(
    names(cast_shadow_shift(airquality, Ozone, Solar.R)),
    c(
      "Ozone",
      "Solar.R",
      "Ozone_NA",
      "Solar.R_NA",
      "Ozone_shift",
      "Solar.R_shift"
    )
  )
  expect_equal(
    names(cast_shadow_shift(airquality, Ozone, Solar.R, Temp)),
    c(
      "Ozone",
      "Solar.R",
      "Temp",
      "Ozone_NA",
      "Solar.R_NA",
      "Temp_NA",
      "Ozone_shift",
      "Solar.R_shift",
      "Temp_shift"
    )
  )
})
