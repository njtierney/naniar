context("gg_miss_fct")

gg_miss_fct_plot <- gg_miss_fct(riskfactors, marital)

test_that("gg_miss_fct works",{
  skip_on_cran()
  skip_on_appveyor()
  skip_on_gh_actions()
  vdiffr::expect_doppelganger("gg_miss_fct",
                              gg_miss_fct_plot)
})
