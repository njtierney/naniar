context("cast_shadow")

test_that("cast_shadow returns a tibble",{
  expect_is(cast_shadow(airquality, Ozone), "tbl_df")
})

test_that("cast_shadow adds the right number of columns",{
  expect_equal(2, ncol(cast_shadow(airquality, Ozone)))
  expect_equal(4, ncol(cast_shadow(airquality, Ozone, Solar.R)))
  expect_equal(6, ncol(cast_shadow(airquality, Ozone, Solar.R, Temp)))
})

test_that("cast_shadow adds a column with suffix '_NA'",{
  expect_equal(names(cast_shadow(airquality, Ozone)),
               c("Ozone","Ozone_NA"))
  expect_equal(names(cast_shadow(airquality, Ozone, Solar.R)),
               c("Ozone","Solar.R", "Ozone_NA", "Solar.R_NA"))
  expect_equal(names(cast_shadow(airquality, Ozone, Solar.R, Temp)),
               c("Ozone","Solar.R","Temp", "Ozone_NA", "Solar.R_NA", "Temp_NA"))
})

test_that("cast_shadow returns a nice error when you don't include variables",{
  expect_error(cast_shadow(airquality))
})
