test_df <- data.frame(x = c(NA,2,3),
                      y = c(1,NA,3),
                      z = c(1,2,3))

test_that("label_miss_1d errors on a NULL entry",{
  expect_error(label_miss_1d(NULL))
})

test_that("label_miss_1d returns a vector of the same length as the input",{
  expect_length(label_miss_1d(test_df$x),
                nrow(test_df))
})

test_that("label_miss_1d returns character vector",{
  expect_type(label_miss_1d(test_df$x),
              "character")
})

test_that("label_miss_1d identifies the correct location of missingness",{
  expect_equal(label_miss_1d(test_df$x),
               c("Missing", "Not Missing", "Not Missing"))
  expect_equal(label_miss_1d(test_df$y),
               c("Not Missing", "Missing", "Not Missing"))
  expect_equal(label_miss_1d(test_df$z),
               c("Not Missing", "Not Missing", "Not Missing"))
})


