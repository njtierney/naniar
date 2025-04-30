test_that("miss_prop_summary returns a data.frame", {
  expect_s3_class(miss_prop_summary(airquality), "data.frame")
})

test_that("miss_prop_summary returns a tibble", {
  expect_equal(
    class(miss_prop_summary(airquality)),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("miss_prop_summary errors when given non dataframe or 0 entry", {
  expect_snapshot(
    error = TRUE,
    miss_prop_summary(0)
  )
  expect_snapshot(
    error = TRUE,
    miss_prop_summary("a")
  )
  expect_snapshot(
    error = TRUE,
    miss_prop_summary(matrix(airquality))
  )
  expect_snapshot(
    error = TRUE,
    miss_prop_summary(NULL)
  )
})

test_that("There are 3 columns", {
  expect_equal(ncol(miss_prop_summary(airquality)), 3)
})

test_that("The columns are named df, var, case", {
  expect_named(miss_prop_summary(airquality), c("df", "var", "case"))
})


aq_group <- dplyr::group_by(airquality, Month)

test_that("miss_prop_summary grouped_df returns a tibble", {
  expect_s3_class(miss_prop_summary(aq_group), "tbl_df")
})

test_that("grouped_df returns 1 more column than regular miss_prop_summary", {
  expect_equal(
    ncol(miss_prop_summary(aq_group)),
    ncol(miss_prop_summary(airquality)) + 1
  )
})

test_that("grouped_df returns a column named 'Month'", {
  expect_identical(
    names(miss_prop_summary(aq_group)),
    c("Month", "df", "var", "case")
  )
})

test_that("grouped_df returns a dataframe with more rows than regular", {
  expect_gt(
    nrow(miss_prop_summary(aq_group)),
    nrow(miss_prop_summary(airquality))
  )
})

test_that("grouped_df returns a column named 'Month' with the right levels", {
  expect_identical(unique(miss_prop_summary(aq_group)$Month), 5:9)
})
