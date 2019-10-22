context("Defuncts")

df <- tibble::tibble(x = c("A", NA))

test_that("miss_var_prop is defunct", {
  expect_error(miss_var_prop(df), class = "defunctError")
})

test_that("complete_var_prop is defunct", {
  expect_error(complete_var_prop(df), class = "defunctError")
})

test_that("miss_var_pct is defunct", {
  expect_error(miss_var_pct(df), class = "defunctError")
})

test_that("complete_var_pct is defunct", {
  expect_error(complete_var_pct(df), class = "defunctError")
})

test_that("miss_case_prop is defunct", {
  expect_error(miss_case_prop(df),, class = "defunctError")
})

test_that("complete_case_prop is defunct", {
  expect_error(complete_case_prop(df), class = "defunctError")
})

test_that("miss_case_pct is defunct", {
  expect_error(miss_case_pct(df), class = "defunctError")
})

test_that("complete_case_pct is defunct", {
  expect_error(complete_case_pct(df), class = "defunctError")
})

test_that("replace_to_na is defunct", {
  expect_error(replace_to_na(df), class = "defunctError")
})
