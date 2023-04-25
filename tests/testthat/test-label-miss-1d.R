test_df <- data.frame(x = c(NA,2,3),
                      y = c(1,NA,3),
                      z = c(1,2,3))

test_that("label_miss_1d errors on a NULL entry",{
  expect_snapshot(
    error = TRUE,
    label_miss_1d(NULL)
    )
})

test_that("label_miss_1d returns a vector of the same length as the input",{
  expect_length(label_miss_1d(test_df$x),
                nrow(test_df))
})

test_that("label_miss_1d returns factor vector",{
  expect_type(label_miss_1d(test_df$x),
              # typeof(factor()) is "integer"
              "integer")
})

test_that("label_miss_1d identifies the correct location of missingness",{
  expect_snapshot(
    label_miss_1d(test_df$x)
  )
  expect_snapshot(
    label_miss_1d(test_df$y)
  )
  expect_snapshot(
    label_miss_1d(test_df$z)
  )
})


