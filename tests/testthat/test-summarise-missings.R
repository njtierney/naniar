context("summarise missingness")

test_that("summarise_missingness returns a data.frame",{
  expect_s3_class(summarise_missingness(airquality),
                  "data.frame")
})

test_that("summarise_missingness returns a tibble",{
  expect_equal(class(summarise_missingness(airquality)),
               c("tbl_df", "tbl", "data.frame"))
})

test_that("summarise_missingness errors when given non dataframe or 0 entry",{
  expect_error(summarise_missingness(0))
  expect_error(summarise_missingness("a"))
  expect_error(summarise_missingness(matrix(airquality)))
  expect_error(summarise_missingness(NULL))
})

test_that("There are 7 columns",{
  expect_equal(ncol(summarise_missingness(airquality)),7)
})

test_that("The columns are named correctly",{
  expect_named(summarise_missingness(airquality),
               c("percent_missing_df",
                 "percent_missing_var",
                 "percent_missing_case",
                 "table_missing_case",
                 "table_missing_var",
                 "summary_missing_var",
                 "summary_missing_case"))
})
