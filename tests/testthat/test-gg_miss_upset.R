context("test-gg_miss_upset.R")

gg_miss_upset_plot <- gg_miss_upset(riskfactors)

test_that("gg_miss_upset works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_upset",
                              gg_miss_upset_plot)
})
