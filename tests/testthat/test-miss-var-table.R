context("miss_var_table tidiers")

test_that("miss_var_table errors on NULL",{
  expect_error(miss_var_table(NULL))
})

test_that("miss_var_table errors when a non-dataframe given",{
  expect_error(miss_var_table(1))
  expect_error(miss_var_table("a"))
  expect_error(miss_var_table(matrix(iris)))
})

test_that("miss_var_table produces a data_frame", {
  expect_is(miss_var_table(airquality), "tbl_df")
})


