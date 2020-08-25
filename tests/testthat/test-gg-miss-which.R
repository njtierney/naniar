context("gg_miss_which")

gg_miss_which_plot <- gg_miss_which(airquality)

test_that("gg_miss_which_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_which",
                              gg_miss_which_plot)
})
