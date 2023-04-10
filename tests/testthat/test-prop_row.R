# is there a more general way to write these kinds of tests?
test_that("rowwise errors for non data frames",{
  expect_error(n_miss_row(numeric(0)))
  expect_error(n_miss_row(NULL))

  expect_error(n_complete_row(numeric(0)))
  expect_error(n_complete_row(NULL))

  expect_error(prop_miss_row(numeric(0)))
  expect_error(prop_miss_row(NULL))

  expect_error(prop_complete_row(numeric(0)))
  expect_error(prop_complete_row(NULL))

})

d10 <- diag_na(10)
ds <- d10
ds[upper.tri(ds)] <- NA

test_that("n/prop_miss_row correctly counts the missings",{
  expect_equal(n_miss_row(d10), rep(1,10))
  expect_equal(n_miss_row(ds), 10:1)
  expect_equal(prop_miss_row(d10), rep(0.1,10))
  expect_equal(prop_miss_row(ds), rev(seq(from = 0.1, to = 1, by = 0.1)))

})

test_that("n/prop_complete_row correctly counts the missings",{
  expect_equal(n_complete_row(d10), rep(9,10))
  expect_equal(n_complete_row(ds), 0:9)
  expect_equal(prop_complete_row(d10), rep(0.9,10))
  expect_equal(prop_complete_row(ds), seq(from = 0.0, to = 0.9, by = 0.1))

})
