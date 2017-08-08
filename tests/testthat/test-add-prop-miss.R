context("add_prop_na")

test_df <- data.frame(x = c(NA,2,3),
                      y = c(1,NA,3),
                      z = c(1,2,3))

df_prop_na <- data.frame(prop_na_all = c(1/3,
                                             1/3,
                                             0))

test_df_answer <- dplyr::bind_cols(test_df,df_prop_na)


test_that("add_prop_na adds a column",{
  expect_equal(ncol(test_df) + 1, ncol(add_prop_na(test_df)))
})

test_that("add_prop adds a column named 'prop_na_all'",{
  expect_equal(names(add_prop_na(test_df)),
               c(names(test_df),"prop_na_all"))
})

test_that("add_prop adds a column named 'prop_na_vars' when a variable is selected",{
  expect_equal(names(add_prop_na(test_df,y)),
               c(names(test_df),"prop_na_vars"))
})

test_that("add_prop_na returns the correct number",{

  expect_equal(add_prop_na(test_df),test_df_answer)

})
