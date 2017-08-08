context("add_any_na")

test_that("add_any_na returns a tibble",{
  expect_is(add_any_na(airquality, Ozone), "tbl_df")
})

test_that("add_any_na adds the right number of columns",{
  expect_equal(ncol(airquality)+1, ncol(add_any_na(airquality, Ozone)))
  expect_equal(ncol(airquality)+1, ncol(add_any_na(airquality, Ozone, Solar.R)))
  expect_equal(ncol(airquality)+1, ncol(add_any_na(airquality, Ozone, Solar.R, Temp)))
})

test_that("add_any_na adds a column with suffix 'any_na_vars' or 'any_na_all'",{
  expect_equal(names(add_any_na(airquality, Ozone)),
               c(names(airquality), "any_na_vars"))
  expect_equal(names(add_any_na(airquality, Ozone, Solar.R, Temp)),
               c(names(airquality), "any_na_vars"))
  expect_equal(names(add_any_na(airquality)),
               c(names(airquality), "any_na_all"))
})

