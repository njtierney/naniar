context("unbind shadow")

nab <- nabular(airquality)

nabu <- unbind_shadow(nab)

test_that("unbind_shadow returns tbl data", {
  expect_equal(class(nabu), c("tbl_df", "tbl", "data.frame"))
})

test_that("unbind_shadow returns right dimensions", {
  expect_equal(nrow(nabu), nrow(airquality))
  expect_equal(ncol(nabu), ncol(airquality))
})
