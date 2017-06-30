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
