vec <- 1:10

test_that("set_n_miss works", {
  expect_equal(n_miss(set_n_miss(vec, 1)), 1)
  expect_equal(n_miss(set_n_miss(vec, 5)), 5)
  expect_equal(n_miss(set_n_miss(vec, 10)), 10)
  expect_equal(n_miss(set_n_miss(vec, 0)), 0)
})

test_that("set_prop_miss works", {
  expect_equal(prop_miss(set_prop_miss(vec, 0.1)), .1)
  expect_equal(prop_miss(set_prop_miss(vec, 0.5)), .5)
  expect_equal(prop_miss(set_prop_miss(vec, 1)), 1)
  expect_equal(prop_miss(set_prop_miss(vec, 0)), 0)
})

test_that("set_n_miss errors appropriately", {
  expect_snapshot_error(
    set_n_miss(vec, -1)
    )
  expect_snapshot_error(
    set_n_miss(vec, "a")
    )
  expect_snapshot_error(
    set_n_miss(vec, 1.5)
    )
  expect_snapshot_error(
    set_n_miss(vec, c(1.5, 2))
    )
})

test_that("set_prop_miss errors appropriately", {
  expect_snapshot_error(
    set_prop_miss(vec, -1)
    )
  expect_snapshot_error(
    set_prop_miss(vec, "a")
    )
  expect_snapshot_error(
    set_prop_miss(vec, 1.5)
    )
  expect_snapshot_error(
    set_prop_miss(vec, c(1.5, 2))
    )
})
