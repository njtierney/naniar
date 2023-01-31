# gg_miss_var plots ------------------------------------------------------------

dat <- tibble::tribble(
  ~air, ~wind, ~water, ~month,
  -99,    NA,  23,     1,
  -98,    NA,  NA,     1,
  25,     30,  21,     2,
  NA,     99,  NA,     2,
  23,     40,  NA,     2
)

gg_miss_var_plot <- gg_miss_var(dat)

test_that("gg_miss_var_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_var",
                              gg_miss_var_plot)
})

gg_miss_var_plot_group <- gg_miss_var(dat, facet = month)

test_that("gg_miss_var_group_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_var_plot_group",
                              gg_miss_var_plot_group)
})


gg_miss_var_plot_pct <- gg_miss_var(dat,
                                    show_pct = TRUE)

test_that("gg_miss_var_pct_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_var_pct",
                              gg_miss_var_plot_pct)
})

gg_miss_var_plot_group_pct <- gg_miss_var(dat,
                                          facet = month,
                                          show_pct = TRUE)

test_that("gg_miss_var_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_var_group_pct",
                              gg_miss_var_plot_group_pct)
})
