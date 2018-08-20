context("add_label_missings")

test_that("add_label_missings returns a tibble",{
  expect_is(add_label_missings(airquality), "tbl_df")
})

test_that("add_label_missings adds the right number of columns",{
  expect_equal(ncol(airquality)+1, ncol(add_label_missings(airquality)))
})

test_that("add_label_missings adds a column with suffix 'any_missing'",{
  expect_equal(names(add_label_missings(airquality)),
               c(names(airquality), "any_missing"))
})


# add some tests for this condition, where ... is added to label shadow
dat <- tibble::tribble(
  ~air, ~wind, ~water,
  -99,    NA,  23,
  -98,    NA,  NA,
  25,     30,  21,
  NA,     99,  NA,
  23,     40,  NA
)

dat_add <- add_label_missings(dat)

dat_add_air <- add_label_missings(dat, air)

dat_add_air_wind <- add_label_missings(dat, air, wind)

test_that("add_label_missings adds the right values to the column", {
  expect_equal(dat_add$any_missing,
               c("Missing", "Missing", "Not Missing", "Missing", "Missing"))
  expect_equal(dat_add_air$any_missing,
               c(rep("Not Missing", 3), "Missing", "Not Missing"))
  expect_equal(dat_add_air_wind$any_missing,
               c(rep("Missing", 2), "Not Missing", "Missing", "Not Missing"))
})

