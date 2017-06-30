context("add_any_miss")

test_that("add_any_miss returns a tibble",{
  expect_is(add_any_miss(airquality, Ozone), "tbl_df")
})

test_that("add_any_miss adds the right number of columns",{
  expect_equal(ncol(airquality)+1, ncol(add_any_miss(airquality, Ozone)))
  expect_equal(ncol(airquality)+1, ncol(add_any_miss(airquality, Ozone, Solar.R)))
  expect_equal(ncol(airquality)+1, ncol(add_any_miss(airquality, Ozone, Solar.R, Temp)))
})

test_that("add_any_miss adds a column with suffix 'any_miss_vars' or 'any_miss_all'",{
  expect_equal(names(add_any_miss(airquality, Ozone)),
               c(names(airquality), "any_miss_vars"))
  expect_equal(names(add_any_miss(airquality, Ozone, Solar.R, Temp)),
               c(names(airquality), "any_miss_vars"))
  expect_equal(names(add_any_miss(airquality)),
               c(names(airquality), "any_miss_all"))
})

