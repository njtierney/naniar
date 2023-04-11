
test_that("impute_below returns NULL when given NULL",{
  expect_null(impute_below(NULL))
})

test_that("impute_below returns an error when given the wrong kind of object",{
  expect_snapshot(
    error = TRUE,
    impute_below(as.POSIXct(111, origin = "1970-01-01"))
    )
})

miss_vec_5 <- c(10,10,9,NA,3)

test_that("impute_below returns NA values less than minimum for one location",{
  expect_lt(impute_below(miss_vec_5)[which_na(miss_vec_5)],
            min(miss_vec_5, na.rm = TRUE))
})

miss_vec_2 <- c(4,NA)
miss_vec_3 <- c(4,NA,NA)
miss_vec_4 <- c(4,NA,NA,NA)

test_that(
  "impute_below returns NA values less than min when only one missing value",{
    expect_lt(impute_below(miss_vec_2)[which_na(miss_vec_2)],
              min(miss_vec_2, na.rm = TRUE))
    expect_lt(min(impute_below(miss_vec_3)[which_na(miss_vec_3)]),
              min(miss_vec_3, na.rm = TRUE))
    expect_lt(min(impute_below(miss_vec_4)[which_na(miss_vec_4)]),
              min(miss_vec_4, na.rm = TRUE))
  })

miss_vec <- rnorm(100)

# add 20 missing values
miss_vec[sample(1:100,20)] <- NA

test_that("impute_below returns NA values less than min for many locations",{
  expect_lt(min(impute_below(miss_vec)[which_na(miss_vec)]),
            min(miss_vec, na.rm = TRUE))
})

test_that("impute_below returns the same input when length == 1",{
  expect_equal(impute_below(1),1)
})

test_vec <- runif(100)
test_that("impute_below returns same input when there are no missing values",{
  expect_equal(impute_below(test_vec),test_vec)
})

# need to add tests for test-success for new classes supported by impute_below

# expect_error(impute_below("c"))
# expect_error(impute_below(iris$Species))

test_that("impute_below prop_below makes shifts bigger",{
  expect_gt(min(impute_below(miss_vec)[which_na(miss_vec)]),
            min(impute_below(miss_vec, prop_below = 0.2)[which_na(miss_vec)])
  )
})

test_that("impute_below prop_below makes shifts bigger",{
  expect_gt(min(impute_below(miss_vec, prop_below = 0.2)[which_na(miss_vec)]),
            min(impute_below(miss_vec, prop_below = 0.4)[which_na(miss_vec)])

  )
})

test_that("impute_below jitter makes shifts bigger",{
  expect_lt(var(impute_below(miss_vec)[which_na(miss_vec)]),
            var(impute_below(miss_vec, jitter = 0.2)[which_na(miss_vec)])
  )
})

test_that("impute_below jitter makes shifts bigger",{
  expect_lt(var(impute_below(miss_vec, jitter = 0.2)[which_na(miss_vec)]),
            var(impute_below(miss_vec, jitter = 0.4)[which_na(miss_vec)])

  )
})

df_inf <-  data.frame(x = c(-Inf,rnorm(2), NA, Inf))

test_that("missing values are replaced in shadow shift",{
  expect_false(anyNA(impute_below(df_inf$x)))
})

test_that("infinite values are maintained in shadow shift",{
  expect_equal(sum(is.infinite(impute_below(df_inf$x))), 2)
})


toy_data <- data.frame(
  x = c(1,2,3,NA,NA,NA,7,8,9),
  y = c(LETTERS[1:7], NA, NA),
  z = c(NA,NA, rnorm(6),NA)
)

test_that("impute_below_all returns NULL when given NULL",{
  expect_equal(impute_below_all(data.frame(NULL)), data.frame(NULL))
})

test_that("impute_below_all errors when given wrong object",{
  expect_snapshot(
    error = TRUE,
    impute_below_all(as.POSIXct(111, origin = "1970-01-01"))
    )
})

miss_vec_5 <- c(10,10,9,NA,3)

test_that("impute_below_all returns NA values less than min for one location",{
  expect_lt(impute_below_all(data.frame(miss_vec_5))[which_na(miss_vec_5), ],
            min(miss_vec_5, na.rm = TRUE))
})

miss_vec_2 <- data.frame(x = c(4,NA))
miss_vec_4 <- data.frame(x = c(4,NA,NA,NA))

test_that("impute_below_all returns NA values < min when only one missing",{
  expect_lt(impute_below_all(miss_vec_2)[which_na(miss_vec_2), ],
            min(miss_vec_2, na.rm = TRUE))
  expect_lt(min(impute_below_all(miss_vec_4)[which_na(miss_vec_4), ]),
            min(miss_vec_4, na.rm = TRUE))
})

miss_vec <- data.frame(x = rnorm(100))

# add 20 missing values
miss_vec[sample(1:100,20), ] <- NA

test_that("impute_below_all returns NA values less than min for many locations",{
  expect_lt(min(impute_below_all(miss_vec)[which_na(miss_vec), ]),
            min(miss_vec, na.rm = TRUE))
})

test_that("impute_below_all returns the same input when length == 1",{
  expect_equal(impute_below_all(data.frame(1)),data.frame(1))
})

test_vec <- data.frame(x = runif(100))
test_that("impute_below_all returns same input when no missings",{
  expect_equal(impute_below_all(test_vec),test_vec)
})


miss_vec <- rnorm(100)

# add 20 missing values
miss_vec[sample(1:100,20)] <- NA

miss_df <- data.frame(miss_vec)

test_that("impute_below_all leaves no NA values",{
  expect_false(
    anyNA(impute_below_all(miss_df)[which_na(miss_df$miss_vec), ])
  )
})

test_that("impute_below_all prop_below makes shifts bigger",{
  expect_gt(
    min(impute_below_all(miss_df)[which_na(miss_df$miss_vec), ]),
    min(impute_below_all(miss_df,
                     prop_below = 0.3)[which_na(miss_df$miss_vec), ])
  )
})

test_that("impute_below_all prop_below makes shifts bigger",{
  expect_gt(min(impute_below_all(miss_df,
                             prop_below = 0.2)[which_na(miss_df$miss_vec), ]),
            min(impute_below_all(miss_df,
                             prop_below = 0.4)[which_na(miss_df$miss_vec), ])

  )
})

test_that("impute_below_all jitter makes shifts bigger",{
  expect_lt(var(impute_below_all(miss_df)[which_na(miss_df$miss_vec), ]),
            var(impute_below_all(miss_df,
                             jitter = 0.2)[which_na(miss_df$miss_vec), ])
  )
})

test_that("impute_below_all jitter makes shifts bigger",{
  expect_lt(var(impute_below_all(miss_df,
                             jitter = 0.2)[which_na(miss_df$miss_vec), ]),
            var(impute_below_all(miss_df,
                             jitter = 0.4)[which_na(miss_df$miss_vec), ])

  )
})

aq_s <- nabular(airquality)

test_that("impute_below works with nabular", {
  expect_type(impute_below(aq_s$Ozone), "double")
  expect_s3_class(impute_below_all(aq_s), "data.frame")
  expect_s3_class(impute_below_at(aq_s,
                                  dplyr::vars(Ozone, Solar.R)), "data.frame")
  expect_s3_class(impute_below_if(aq_s, is.numeric), "data.frame")
})
