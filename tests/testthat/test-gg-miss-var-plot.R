# gg_miss_var plots ------------------------------------------------------------

context("gg_miss_var_plot")

gg_miss_var_plot <- gg_miss_var(airquality)

test_that("gg_miss_var_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_var",
                              gg_miss_var_plot)
})

gg_miss_var_plot_group <- gg_miss_var(airquality, facet = Month)

test_that("gg_miss_var_group_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_var_plot_group",
                              gg_miss_var_plot_group)
})


gg_miss_var_plot_pct <- gg_miss_var(airquality,
                                    show_pct = TRUE)

test_that("gg_miss_var_pct_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_var_pct",
                              gg_miss_var_plot_pct)
})

gg_miss_var_plot_group_pct <- gg_miss_var(airquality,
                                          facet = Month,
                                          show_pct = TRUE)

test_that("gg_miss_var_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_var_group_pct",
                              gg_miss_var_plot_group_pct)
})
