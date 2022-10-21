df <- data.frame(x = c(NA, 1:4),
                 y = c(NA, NA, 1:3))

nab <- nabular(df)
nabu <- unbind_shadow(nab)

test_that("unbind_shadow returns tibble", {
  expect_is(nabu, c("tbl_df"))
})

test_that("unbind_shadow returns right dimensions", {
  expect_equal(nrow(nabu), nrow(df))
  expect_equal(ncol(nabu), ncol(df))
})
