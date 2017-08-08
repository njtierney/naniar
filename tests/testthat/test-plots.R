context("naniar-plots")

library(ggplot2)
geom_na_point_plot <-
  ggplot(airquality,
         aes(x = Solar.R,
             y = Ozone)) +
  geom_na_point()

test_that("geom_na_point works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("geom_na_point", geom_na_point_plot)
})


gg_na_case_plot <- gg_na_case(airquality)

test_that("gg_na_case_works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("gg_na_case",
                              gg_na_case_plot)
})

gg_na_var_plot <- gg_na_var(airquality)

test_that("gg_na_var_works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("gg_na_var",
                              gg_na_var_plot)
})

gg_na_which_plot <- gg_na_which(airquality)

test_that("gg_na_which_works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("gg_na_which",
                              gg_na_which_plot)
})

gg_na_fct_plot <- gg_na_fct(riskfactors, marital)

test_that("gg_na_fct works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("gg_na_fct",
                              gg_na_fct_plot)
})

gg_na_span_plot <- gg_na_span(pedestrian, hourly_counts, 4000)

test_that("gg_na_span works",{
  skip_on_cran()
  vdiffr::expect_doppelganger("gg_na_span",
                              gg_na_span_plot)
})
