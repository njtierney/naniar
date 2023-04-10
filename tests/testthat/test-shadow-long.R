aq_shadow <- nabular(airquality)

aq_sh_long <- shadow_long(aq_shadow)

test_that("shadow_long returns data of the right dimensions", {
  expect_equal(dim(aq_sh_long), c(918, 4))
})

test_that("shadow_long returns data with the right names", {
  expect_equal(names(aq_sh_long),
               c("variable", "value", "variable_NA", "value_NA"))
})
library(purrr)

test_that("shadow_long returns right data class", {
  expect_equal(as.character(map_chr(aq_sh_long, class)),
               c("character", "numeric", "character", "factor"))
})

aq_sh_long_ozone <- shadow_long(aq_shadow, Ozone)

test_that("shadow_long returns data with right dimensions when filtered", {
  expect_equal(dim(aq_sh_long_ozone), c(153, 4))
})

test_that("shadow_long returns data with right names when filtered", {
  expect_equal(names(aq_sh_long_ozone),
               c("variable", "value", "variable_NA", "value_NA"))
})

test_that("shadow_long returns right data class when filtered", {
  expect_equal(as.character(map_chr(aq_sh_long_ozone, class)),
               c("character", "numeric", "character", "factor"))
})


aq_sh_long_ozone_solar <- shadow_long(aq_shadow, Ozone, Solar.R)

test_that("shadow_long returns data with right dimensions when filtered", {
  expect_equal(dim(aq_sh_long_ozone_solar), c(306, 4))
})

test_that("shadow_long returns data with right names when filtered", {
  expect_equal(names(aq_sh_long_ozone_solar),
               c("variable", "value", "variable_NA", "value_NA"))
})

test_that("shadow_long returns right data class when filtered", {
  expect_equal(as.character(map_chr(aq_sh_long_ozone_solar, class)),
               c("character", "numeric", "character", "factor"))
})

