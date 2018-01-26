context("naniar-plots")

library(ggplot2)
geom_miss_point_plot <-
  ggplot(airquality,
         aes(x = Solar.R,
             y = Ozone)) +
  geom_miss_point()

test_that("geom_miss_point works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("geom_miss_point", geom_miss_point_plot)
})

# gg_miss_case plots -----------------------------------------------------------

gg_miss_case_plot <- gg_miss_case(airquality)

test_that("gg_miss_case_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case",
                              gg_miss_case_plot)
})

gg_miss_case_plot_group <- gg_miss_case(airquality, group = Month)

test_that("gg_miss_case_group_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case_group",
                              gg_miss_case_plot_group)
})

gg_miss_case_plot_sort <- gg_miss_case(airquality, order_cases = TRUE)

test_that("gg_miss_case_sort_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case_sort",
                              gg_miss_case_plot_sort)
})

gg_miss_case_plot_order_group_sort <- gg_miss_case(airquality,
                                                   group = Month,
                                                   order_cases = TRUE)

test_that("gg_miss_case_group_and_sort_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case_group_and_sort",
                              gg_miss_case_plot_order_group_sort)
})

# gg_miss_var plots ------------------------------------------------------------

gg_miss_var_plot <- gg_miss_var(airquality)

test_that("gg_miss_var_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_var",
                              gg_miss_var_plot)
})

gg_miss_var_plot_group <- gg_miss_var(airquality, group = Month)

test_that("gg_miss_var_group_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_var_plot_group",
                              gg_miss_var_plot_group)
})


gg_miss_var_plot_pct <- gg_miss_var(airquality,
                                    show_pct = TRUE)

test_that("gg_miss_var_pct_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_var_pct",
                              gg_miss_var_plot_pct)
})

gg_miss_var_plot_group_pct <- gg_miss_var(airquality,
                                      group = Month,
                                      show_pct = TRUE)

test_that("gg_miss_var_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_var_group_pct",
                              gg_miss_var_plot_group_pct)
})

gg_miss_which_plot <- gg_miss_which(airquality)

test_that("gg_miss_which_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_which",
                              gg_miss_which_plot)
})

gg_miss_fct_plot <- gg_miss_fct(riskfactors, marital)

test_that("gg_miss_fct works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_fct",
                              gg_miss_fct_plot)
})

gg_miss_span_plot <- gg_miss_span(pedestrian, hourly_counts, 4000)

test_that("gg_miss_span works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_span",
                              gg_miss_span_plot)
})

