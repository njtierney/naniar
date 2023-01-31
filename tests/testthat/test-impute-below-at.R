miss_vec <- rnorm(100)

# add 20 missing values
miss_vec[sample(1:100,20)] <- NA

miss_df <- data.frame(miss_vec)

test_that("impute_below_at leaves no NA values",{
  expect_false(
    anyNA(impute_below_at(miss_df,
                          "miss_vec")[which(is.na(miss_df$miss_vec)), ])
  )
})

test_that("impute_below_at works with vars",{
  expect_false(
    anyNA(impute_below_at(miss_df,
                          dplyr::vars(miss_vec))[which(is.na(miss_df$miss_vec)), ])
  )
})

miss_vec <- rnorm(100)

# add 20 missing values
miss_vec[sample(1:100,20)] <- NA

miss_df <- data.frame(miss_vec)

test_that("impute_below_if leaves no NA values",{
  expect_false(
    anyNA(impute_below_if(airquality,
                          .predicate = is.numeric))
  )
})
