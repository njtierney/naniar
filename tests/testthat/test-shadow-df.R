context("test shadow_df")

test_that("shadow_df is a data_frame",{
  expect_is(shadow_df(iris),"data.frame")
})

test_that("shadow_df contains logical data",{
  expect_equal(sum(purrr:::map(shadow_df(iris),class) == "logical"),
               ncol(iris))
})


