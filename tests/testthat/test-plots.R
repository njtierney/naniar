context("naniar-plots")

library(ggplot2)
geom_miss_point_plot <-
  ggplot(airquality,
         aes(x = Solar.R,
             y = Ozone)) +
  geom_miss_point()

test_that("geom_miss_point works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("geom_miss_point", geom_miss_point_plot)
})


gg_miss_case_plot <- gg_miss_case(airquality)

test_that("gg_miss_case_works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("gg_miss_case",
                              gg_miss_case_plot)
})

gg_miss_var_plot <- gg_miss_var(airquality)

test_that("gg_miss_var_works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("gg_miss_var",
                              gg_miss_var_plot)
})

gg_miss_which_plot <- gg_miss_which(airquality)

test_that("gg_miss_which_works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("gg_miss_which",
                              gg_miss_which_plot)
})

gg_miss_fct_plot <- gg_miss_fct(riskfactors, marital)

test_that("gg_miss_fct works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("gg_miss_fct",
                              gg_miss_fct_plot)
})

gg_miss_span_plot <- gg_miss_span(pedestrian, hourly_counts, 4000)

test_that("gg_miss_span works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("gg_miss_span",
                              gg_miss_span_plot)
})
