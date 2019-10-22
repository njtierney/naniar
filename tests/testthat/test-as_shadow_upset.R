context("as_shadow_upset words")

test_that("as_shadow_upset errors when given datasets with <= 1 variables", {
  expect_error(as_shadow_upset(diag_na(1)))
  expect_error(as_shadow_upset(data.frame(x = NA)))
  expect_error(as_shadow_upset(data.frame(numeric(0))))
})

test_that("as_shadow_upset returns a data.frame",{
  expect_s3_class(as_shadow_upset(airquality), "data.frame")
})

test_that("as_shadow_upset errors when given non dataframe or 0 entry",{
  expect_error(as_shadow_upset(0))
  expect_error(as_shadow_upset("a"))
  expect_error(as_shadow_upset(matrix(airquality)))
  expect_error(as_shadow_upset(NULL))
})

test_that("The number of rows are the same after using as_shadow_upset",{
  expect_equal(nrow(as_shadow_upset(airquality)),nrow(airquality))
})

test_that("The number of columns are the same after using bind_shadow",{
  expect_equal(ncol(as_shadow_upset(airquality)),ncol(airquality))
})

aq_u <- as_shadow_upset(airquality)
test_that("as_shadow_upset returns things of type integer", {
  expect_true(all(purrr::map_chr(aq_u,class) == "integer"))
})
