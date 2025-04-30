test_df <- data.frame(x = c(NA, 2, 3), y = c(1, NA, 3), z = c(1, 2, 3))

df_prop_miss <- data.frame(prop_miss_all = c(1 / 3, 1 / 3, 0))

test_df_answer <- dplyr::bind_cols(test_df, df_prop_miss)


test_that("add_prop_miss adds a column", {
  expect_equal(ncol(test_df) + 1, ncol(add_prop_miss(test_df)))
})

test_that("add_prop adds a column named 'prop_miss_all'", {
  expect_equal(
    names(add_prop_miss(test_df)),
    c(names(test_df), "prop_miss_all")
  )
})

test_that("add_prop adds a column named 'prop_miss_vars' when a variable is selected", {
  expect_equal(
    names(add_prop_miss(test_df, y)),
    c(names(test_df), "prop_miss_vars")
  )
})

test_that("add_prop_miss returns the correct number", {
  expect_equal(add_prop_miss(test_df), test_df_answer)
})
