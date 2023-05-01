ocean_shadow <- nabular(oceanbuoys)

test_that("shadow_long returns the right dimensions and names etc", {
  expect_snapshot(
    shadow_long(ocean_shadow)
  )
})

test_that("shadow_long works gives the classes with function value transform", {
  expect_snapshot(
    shadow_long(ocean_shadow,
                fn_value_transform = as.numeric)
  )
})

test_that("shadow_long returns right dimensions, names, etc when filtered", {
  expect_snapshot(
    shadow_long(ocean_shadow, air_temp_c, humidity)
  )
})

test_that("shadow_long returns right dimensions, names, etc when filtered with function value transform", {
  expect_snapshot(
    shadow_long(ocean_shadow,
                air_temp_c,
                humidity,
                fn_value_transform = as.numeric
  )
  )
})
