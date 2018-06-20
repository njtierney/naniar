context("miss_case_table tidiers")

test_that("miss_case_table errors on NULL",{
  expect_error(miss_case_table(NULL))
})

test_that("miss_case_table errors when a non-dataframe given",{
  expect_error(miss_case_table(1))
  expect_error(miss_case_table("a"))
  expect_error(miss_case_table(matrix(iris)))
})

test_that("miss_case_table produces a data_frame", {
  expect_is(miss_case_table(airquality), "tbl_df")
})

aq_group <- dplyr::group_by(airquality, Month)


test_that("grouped_df returns a tibble miss_case_table", {
  expect_is(miss_case_table(aq_group), "tbl_df")
})

test_that("grouped_df returns 1 more column than regular miss_case_table", {
  expect_equal(ncol(miss_case_table(aq_group)),
               ncol(miss_case_table(airquality))+1)
})

test_that("grouped_df returns a column named 'Month'", {
  expect_identical(names(miss_case_table(aq_group)),
                   c("Month", "n_miss_in_case", "n_cases","pct_cases"))
})

test_that("grouped_df returns a dataframe with more rows than regular", {
  expect_gt(nrow(miss_case_table(aq_group)),
            nrow(miss_case_table(airquality))
            )
})

test_that("grouped_df returns a column named 'Month' with the right levels", {
  expect_identical(unique(miss_case_table(aq_group)$Month),
                   5:9)
})

