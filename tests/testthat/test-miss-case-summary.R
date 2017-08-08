context("na_case_summary tidiers")

test_that("na_case_summary errors on NULL",{
  expect_error(na_case_summary(NULL))
})

test_that("na_case_summary errors when a non-dataframe given",{

  expect_error(na_case_summary(1))
  expect_error(na_case_summary("a"))
  expect_error(na_case_summary(matrix(iris)))
})

test_that("na_case_summary produces a data_frame", {
  expect_is(na_case_summary(airquality), "tbl_df")
})

# grouping

aq_group <- dplyr::group_by(airquality, Month)

test_that("na_case_summary on grouped_df returns a tibble", {
  expect_is(na_case_summary(aq_group), "tbl_df")
})

test_that("grouped_df returns 1 more column than regular na_case_summary", {
  expect_equal(ncol(na_case_summary(aq_group)),
                   ncol(na_case_summary(airquality))+1)
})

test_that("grouped_df returns a column named 'Month'", {
  expect_identical(names(na_case_summary(aq_group)),
                   c("Month", "case", "n_missing","percent"))
})

test_that("grouped_df returns a column named 'Month' with the right levels", {
  expect_identical(unique(na_case_summary(aq_group)$Month),
                   5:9)
})
