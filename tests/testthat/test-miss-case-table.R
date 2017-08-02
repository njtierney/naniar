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

# miss_var_summary(airquality)
# miss_var_summary(aq_group)

test_that("grouped_df returns a tibble miss_var_summary", {
  expect_is(miss_var_summary(aq_group), "tbl_df")
})

test_that("grouped_df returns 1 more column than regular miss_var_summary", {
  expect_equal(ncol(miss_var_summary(aq_group)),
               ncol(miss_var_summary(airquality))+1)
})

test_that("grouped_df returns a column named 'Month'", {
  expect_identical(names(miss_var_summary(aq_group)),
                   c("Month", "case", "n_missing","percent"))
})

test_that("grouped_df returns a dataframe with more rows than regular", {
  expect_gt(nrow(miss_var_summary(airquality))
    nrow(miss_var_summary(aq_group)),
            )
})

