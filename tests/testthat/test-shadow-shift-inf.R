df_inf <-  data.frame(x = c(-Inf,rnorm(2), NA, Inf))

test_that("missing values are replaced in shadow shift",{
  expect_false(anyNA(shadow_shift(df_inf$x)))
})

test_that("infinite values are maintained in shadow shift",{
  expect_equal(sum(is.infinite(shadow_shift(df_inf$x))), 2)
})
