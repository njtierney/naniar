test_that("label_shadow returns character", {
  expect_type(label_shadow(airquality), "character")
})

test_that("label_shadow adds the right number of elements", {
  expect_equal(nrow(airquality), length(label_shadow(airquality)))
})

aq_shadow <- airquality %>% add_shadow(Ozone)

test_that("label_shadow contains 'Not Missing' and 'Missing'", {
  expect_equal(unique(label_shadow(aq_shadow)), c("Not Missing", "Missing"))
})
