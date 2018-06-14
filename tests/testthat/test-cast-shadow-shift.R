context("cast_shadow_shift")

test_that("cast_shadow_shift returns a tibble",{
  expect_is(cast_shadow_shift(airquality, Ozone), "tbl_df")
})

test_that("cast_shadow_shift adds the right number of columns",{
  expect_equal(3, ncol(cast_shadow_shift(airquality, Ozone)))
  expect_equal(6, ncol(cast_shadow_shift(airquality, Ozone, Solar.R)))
  expect_equal(9, ncol(cast_shadow_shift(airquality, Ozone, Solar.R, Temp)))
})

test_that("cast_shadow_shift adds a column with suffix '_NA' AND '_shift'",{
  expect_equal(names(cast_shadow_shift(airquality, Ozone)),
               c("Ozone","Ozone_NA", "Ozone_shift"))
  expect_equal(names(cast_shadow_shift(airquality, Ozone, Solar.R)),
               c("Ozone","Solar.R",
                 "Ozone_NA", "Solar.R_NA",
                 "Ozone_shift", "Solar.R_shift"))
  expect_equal(names(cast_shadow_shift(airquality, Ozone, Solar.R, Temp)),
               c("Ozone","Solar.R","Temp",
                 "Ozone_NA", "Solar.R_NA", "Temp_NA",
                 "Ozone_shift", "Solar.R_shift", "Temp_shift"))
})

test_that(
  "cast_shadow_shift returns nice error when variables aren't included",{
  expect_error(cast_shadow_shift(airquality))
})
