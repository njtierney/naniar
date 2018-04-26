context("add_shadow")

aq_shadow <- airquality %>% add_shadow(Ozone)

test_that("add_shadow returns a tibble",{
  expect_is(add_shadow(airquality, Ozone), "tbl_df")
})

test_that(
  "add_shadow returns a nice error message when no variables are provided",{
  expect_error(add_shadow(airquality))
})

test_that("add_shadow adds the right number of columns",{
  expect_equal(ncol(airquality)+1, ncol(add_shadow(airquality, Ozone)))
})

test_that("add_shadow adds a column with suffix '_NA'",{
  expect_equal(names(add_shadow(airquality, Ozone)),
               c(names(airquality), "Ozone_NA"))
})
