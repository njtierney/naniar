context("shadow shift")

test_that("shadow_shift returns NULL when given NULL",{
  expect_null(shadow_shift(NULL))
})

test_that("shadow_shift returns an error when given the wrong kind of object",{
  expect_error(shadow_shift("c"))
  # expect_error(shadow_shift(iris$Species))
  expect_error(shadow_shift(as.character(iris$Species)))
})


miss_vec_5 <- c(10,10,9,NA,3)

which_miss <- function(x) which(is.na(x))



test_that("shadow_shift returns NA values less than minimum for one location",{
  expect_lt(shadow_shift(miss_vec_5)[which_miss(miss_vec_5)],
                   min(miss_vec_5, na.rm = TRUE))
})

miss_vec_2 <- c(4,NA)
miss_vec_3 <- c(4,NA,NA)
miss_vec_4 <- c(4,NA,NA,NA)

test_that("shadow_shift returns NA values less than minimum when there is only one missing value",{
  expect_lt(shadow_shift(miss_vec_2)[which_miss(miss_vec_2)],
            min(miss_vec_2, na.rm = TRUE))
  expect_lt(min(shadow_shift(miss_vec_3)[which_miss(miss_vec_3)]),
            min(miss_vec_3, na.rm = TRUE))
  expect_lt(min(shadow_shift(miss_vec_4)[which_miss(miss_vec_4)]),
            min(miss_vec_4, na.rm = TRUE))
})

miss_vec <- rnorm(100)

# add 20 missing values
miss_vec[sample(1:100,20)] <- NA

test_that("shadow_shift returns NA values less than min for many locations",{
  expect_lt(min(shadow_shift(miss_vec)[which_miss(miss_vec)]),
                   min(miss_vec, na.rm = TRUE))
})

test_that("shadow_shift returns the same input when length == 1",{
  expect_equal(shadow_shift(1),1)
})

test_vec <- runif(100)
test_that("shadow_shift returns same input when there are no missing values",{
  expect_equal(shadow_shift(test_vec),test_vec)
})
