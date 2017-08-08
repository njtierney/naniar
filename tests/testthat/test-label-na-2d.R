context("Label 2d missings")

test_df <- data.frame(x = c(NA,2,3),
                      y = c(1,NA,3),
                      z = c(1,2,3))

test_that("label_na_2d errors on the first NULL entry",{
  expect_error(label_na_2d(NULL, 3))
})

test_that("label_na_2d errors on the second NULL entry",{
  expect_error(label_na_2d(3, NULL))
})

test_that("label_na_2d errors when both are NULL",{
  expect_error(label_na_2d(NULL, NULL))
})

test_that("label_na_2d returns a vector of the same length as the input",{
  expect_length(label_na_2d(test_df$x, test_df$y),
                nrow(test_df))
})

test_that("label_na_2d returns character vector",{
  expect_type(label_na_2d(test_df$x, test_df$y),
              "character")
})


test_that("label_na_2d identifies the correct location of missingness",{
  expect_equal(label_na_2d(test_df$x, test_df$y),
               c("Missing", "Missing", "Not Missing"))
  expect_equal(label_na_2d(test_df$y, test_df$z),
               c("Not Missing", "Missing", "Not Missing"))
  expect_equal(label_na_2d(test_df$x, test_df$z),
               c("Missing", "Not Missing", "Not Missing"))
})

