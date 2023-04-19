df <- tibble::tibble(x = c("A", NA))

test_that("miss_var_prop is defunct", {
  expect_snapshot(
    error = TRUE,
    miss_var_prop(df)
  )
})

test_that("complete_var_prop is defunct", {
  expect_snapshot(
    error = TRUE,
    complete_var_prop(df)
  )
})

test_that("miss_var_pct is defunct", {
  expect_snapshot(
    error = TRUE,
    miss_var_pct(df)
  )
})

test_that("complete_var_pct is defunct", {
  expect_snapshot(
    error = TRUE,
    complete_var_pct(df)
  )
})

test_that("miss_case_prop is defunct", {
  expect_snapshot(
    error = TRUE,
    miss_case_prop(df)
  )
})

test_that("complete_case_prop is defunct", {
  expect_snapshot(
    error = TRUE,
    complete_case_prop(df)
  )
})

test_that("miss_case_pct is defunct", {
  expect_snapshot(
    error = TRUE,
    miss_case_pct(df)
  )
})

test_that("complete_case_pct is defunct", {
  expect_snapshot(
    error = TRUE,
    complete_case_pct(df)
  )
})

test_that("replace_to_na is defunct", {
  expect_snapshot(
    error = TRUE,
    replace_to_na(df)
  )
})
