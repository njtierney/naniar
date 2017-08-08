context("na_var_table tidiers")

test_that("na_var_table errors on NULL",{
  expect_error(na_var_table(NULL))
})

test_that("na_var_table errors when a non-dataframe given",{
  expect_error(na_var_table(1))
  expect_error(na_var_table("a"))
  expect_error(na_var_table(matrix(iris)))
})

test_that("na_var_table produces a data_frame", {
  expect_is(na_var_table(airquality), "tbl_df")
})

aq_group <- dplyr::group_by(airquality, Month)

test_that("na_var_table grouped_df returns a tibble", {
  expect_is(na_var_table(aq_group), "tbl_df")
})

test_that("grouped_df returns 1 more column than regular na_var_table", {
  expect_equal(ncol(na_var_table(aq_group)),
               ncol(na_var_table(airquality))+1)
})

test_that("grouped_df returns a column named 'Month'", {
  expect_identical(names(na_var_table(aq_group)),
                   c("Month", "n_missing_in_var", "n_vars","percent"))
})

test_that("grouped_df returns a dataframe with more rows than regular", {
  expect_gt(nrow(na_var_table(aq_group)),
            nrow(na_var_table(airquality)))
})

test_that("grouped_df returns a column named 'Month' with the right levels", {
  expect_identical(unique(na_var_table(aq_group)$Month),
                   5:9)
})

