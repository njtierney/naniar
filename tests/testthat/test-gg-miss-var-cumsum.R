# gg_miss_var_cumsum plots ----------------------------------------------------

context("gg_miss_var_cumsum")

gg_miss_var_cumsum_plot <- gg_miss_var_cumsum(airquality)

test_that("gg_miss_var_cumsum_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_var_cumsum",
                              gg_miss_var_cumsum_plot)
})
