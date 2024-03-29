test_that("miss_summary returns a data.frame", {
  expect_s3_class(
    miss_summary(airquality),
    "data.frame"
  )
})

test_that("miss_summary returns a tibble", {
  expect_equal(
    class(miss_summary(airquality)),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("miss_summary errors when given non dataframe or 0 entry", {
  expect_snapshot(
    error = TRUE,
    miss_summary(0)
  )
  expect_snapshot(
    error = TRUE,
    miss_summary("a")
  )
  expect_snapshot(
    error = TRUE,
    miss_summary(matrix(airquality))
  )
  expect_snapshot(
    error = TRUE,
    miss_summary(NULL)
  )
})

test_that("There are 7 columns", {
  expect_equal(ncol(miss_summary(airquality)), 7)
})

test_that("The columns are named correctly", {
  expect_named(
    miss_summary(airquality),
    c(
      "miss_df_prop",
      "miss_var_prop",
      "miss_case_prop",
      "miss_case_table",
      "miss_var_table",
      "miss_var_summary",
      "miss_case_summary"
    )
  )
})
