context("na_summary")

test_that("na_summary returns a data.frame",{
  expect_s3_class(na_summary(airquality),
                  "data.frame")
})

test_that("na_summary returns a tibble",{
  expect_equal(class(na_summary(airquality)),
               c("tbl_df", "tbl", "data.frame"))
})

test_that("na_summary errors when given non dataframe or 0 entry",{
  expect_error(na_summary(0))
  expect_error(na_summary("a"))
  expect_error(na_summary(matrix(airquality)))
  expect_error(na_summary(NULL))
})

test_that("There are 7 columns",{
  expect_equal(ncol(na_summary(airquality)),9)
})

test_that("The columns are named correctly",{
  expect_named(na_summary(airquality),
               c("na_df_prop",
                 "na_var_prop",
                 "na_case_prop",
                 "na_case_table",
                 "na_var_table",
                 "na_var_summary",
                 "na_case_summary",
                 "na_var_cumsum",
                 "na_case_cumsum"))
})

# need to add more of the missingness summaries here, along with tests for this
# function
