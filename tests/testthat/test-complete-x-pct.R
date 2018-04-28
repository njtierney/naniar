context("complete_case/var_pct/prop tidiers")

test_that("complete_*_pct errors on NULL",{
  expect_error(prop_complete(NULL))
  expect_error(complete_var_pct(NULL))
  expect_error(complete_case_pct(NULL))
})

test_that("complete_var/case_pct errors when a non-dataframe given",{
  expect_error(complete_var_pct(1))
  expect_error(complete_var_pct("a"))
  expect_error(complete_var_pct(matrix(iris)))

  expect_error(complete_case_pct(1))
  expect_error(complete_case_pct("a"))
  expect_error(complete_case_pct(matrix(iris)))
})

test_that("complete_*_pct produces a single, numeric number", {

  expect_length(prop_complete(airquality), 1)
  expect_type(prop_complete(airquality), "double")

  expect_length(complete_var_pct(airquality), 1)
  expect_type(complete_var_pct(airquality), "double")

  expect_length(complete_case_pct(airquality), 1)
  expect_type(complete_case_pct(airquality), "double")
})

test_that("complete_* is the appropriate complement", {

  sum_props <- prop_complete(airquality) + prop_miss(airquality)

  expect_equivalent(sum_props, 1)

  sum_pcts <- complete_var_pct(airquality) + miss_var_pct(airquality)

  expect_equivalent(sum_pcts, 100)

  sum_var_props <- complete_var_prop(airquality) + miss_var_prop(airquality)

  expect_equivalent(sum_var_props, 1)

  sum_case_pct <- complete_case_pct(airquality) + miss_case_pct(airquality)

  expect_equivalent(sum_case_pct, 100)

  sum_case_prop <- complete_case_prop(airquality) + miss_case_prop(airquality)

  expect_equivalent(sum_case_prop, 1)

})
