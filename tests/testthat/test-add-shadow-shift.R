test_df <- data.frame(
  x = c(NA, 2, 3),
  y = c(1, NA, 3),
  z = c(1, 2, 3),
  xi = factor(c(4, 5, 6))
)

df_prop_miss <- data.frame(prop_miss_all = c(1 / 3, 1 / 3, 0))

test_df_answer <- dplyr::bind_cols(test_df, df_prop_miss)

test_that("add_shadow_shift returns a tibble", {
  expect_s3_class(add_shadow_shift(test_df), "tbl_df")
})

test_that("add_shadow_shift doubles the number of columns when default used", {
  expect_equal(ncol(test_df) * 2, ncol(add_shadow_shift(test_df)))
})

test_that("add_shadow_shift adds one extra columns when one var used", {
  expect_equal(ncol(test_df) + 1, ncol(add_shadow_shift(test_df, xi)))
})

test_that("add_shadow_shift adds a column named 'xi_shift'", {
  expect_equal(
    names(add_shadow_shift(test_df, xi)),
    c(names(test_df), "xi_shift")
  )
})

test_that("add_shadow_shift adds two columns named 'xi_shift' and 'x_shift' ", {
  expect_equal(
    names(add_shadow_shift(test_df, xi, x)),
    c(names(test_df), "xi_shift", "x_shift")
  )
})
