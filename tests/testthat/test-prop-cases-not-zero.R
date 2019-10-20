mdf <- data.frame(x = NA)

test_that("prop missing / complete are 0 or 1 where there is one variable", {
  expect_equal(prop_miss_case(mdf), 1)
  expect_equal(n_case_complete(mdf), 0)
  expect_equal(prop_complete_case(mdf), 0)
})

df_diag_na <- diag_na(10)

test_that("prop missing / complete are 0 or 1 where no complete cases", {
  expect_equal(prop_miss_case(df_diag_na), 1)
  expect_equal(n_case_complete(df_diag_na), 0)
  expect_equal(prop_complete_case(df_diag_na), 0)
})

# This tests against
bad_air_quality <- tibble::tribble(
  ~Ozone, ~Solar.R, ~Wind, ~Temp, ~Month, ~Day,
  NA,      190,   7.4,    67,      5,    1,
  36,       NA,     8,    72,      5,    2,
  12,      149,    NA,    74,      5,    3,
  18,      313,  11.5,    NA,      5,    4,
  NA,       NA,  14.3,    56,     NA,    5,
  28,       NA,  14.9,    66,      5,   NA,
  NA,      190,   7.4,    67,      5,    1,
  36,       NA,     8,    72,      5,    2,
  12,      149,    NA,    74,      5,    3,
  18,      313,  11.5,    NA,      5,    4,
  NA,       NA,  14.3,    56,     NA,    5,
  28,       NA,  14.9,    66,      5,   NA
)

library(dplyr)
library(tibble)

bad_na_df <- bad_air_quality %>%
  summarise(n_missing = n_case_miss(.),
            n_complete = n_case_complete(.),
            prop_missing = prop_miss_case(.),
            prop_complete = prop_complete_case(.))

expected_bad_na_df <- tibble(
  n_missing = 12L,
  n_complete = 0L,
  prop_complete = 0,
  prop_missing = 1
)

test_that("prop_miss_case returns same as mean_",{
  expect_equal(bad_na_df, expected_bad_na_df)
})

