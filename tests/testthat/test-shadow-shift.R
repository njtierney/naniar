context("shadow shift")

test_that("shadow_shift returns NULL when given NULL",{
  expect_null(shadow_shift(NULL))
})

miss_vec_5 <- c(10,10,9,NA,3)

which_miss <- function(x) which(is.na(x))

test_that("shadow_shift returns NA values less than minimum for one location",{
  expect_lt(shadow_shift(miss_vec_5)[which_miss(miss_vec_5)],
                   min(miss_vec_5, na.rm = TRUE))
})

#
# miss_vec_5 <- c(10,10,9,NA,3)
#
# which_miss <- function(x) which(is.na(x))
#
# test_that("shadow_shift returns NA values less than minimum for many locations",{
#   expect_lt(shadow_shift(miss_vec_5)[which_miss(miss_vec_5)],
#                    min(miss_vec_5, na.rm = TRUE))
# })

test_that("shadow_shift returns the same input when length == 1",{
  expect_equal(shadow_shift(1),1)
})

test_vec <- runif(100)
test_that("shadow_shift returns same input when there are no missing values",{
  expect_equal(shadow_shift(test_vec),test_vec)
})
