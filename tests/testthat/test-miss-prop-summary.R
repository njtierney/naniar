context("na_prop_summary")

test_that("na_prop_summary returns a data.frame",{
  expect_s3_class(na_prop_summary(airquality),
                  "data.frame")
})

test_that("na_prop_summary returns a tibble",{
  expect_equal(class(na_prop_summary(airquality)),
               c("tbl_df", "tbl", "data.frame"))
})

test_that("na_prop_summary errors when given non dataframe or 0 entry",{
  expect_error(na_prop_summary(0))
  expect_error(na_prop_summary("a"))
  expect_error(na_prop_summary(matrix(airquality)))
  expect_error(na_prop_summary(NULL))
})

test_that("There are 3 columns",{
  expect_equal(ncol(na_prop_summary(airquality)),3)
})

test_that("The columns are named df, var, case",{
  expect_named(na_prop_summary(airquality),c("df","var","case"))
})


aq_group <- dplyr::group_by(airquality, Month)

test_that("na_prop_summary grouped_df returns a tibble", {
  expect_is(na_prop_summary(aq_group), "tbl_df")
})

test_that("grouped_df returns 1 more column than regular na_prop_summary", {
  expect_equal(ncol(na_prop_summary(aq_group)),
               ncol(na_prop_summary(airquality))+1)
})

test_that("grouped_df returns a column named 'Month'", {
  expect_identical(names(na_prop_summary(aq_group)),
                   c("Month", "df", "var","case"))
})

test_that("grouped_df returns a dataframe with more rows than regular", {
  expect_gt(nrow(na_prop_summary(aq_group)),
            nrow(na_prop_summary(airquality)))
  })

test_that("grouped_df returns a column named 'Month' with the right levels", {
  expect_identical(unique(na_prop_summary(aq_group)$Month),
                   5:9)
})

