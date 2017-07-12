context("narnia-plots")

library(ggplot2)
geom_missing_point_plot <-
  ggplot(airquality,
         aes(x = Solar.R,
             y = Ozone)) +
  geom_missing_point()

test_that("geom_miss_point works",{
  vdiffr::expect_doppelganger("geom_missing_point", geom_missing_point_plot)
})


gg_miss_case_plot <- gg_miss_case(airquality)

test_that("gg_miss_case_works",{
  vdiffr::expect_doppelganger("gg_miss_case",
                              gg_miss_case_plot)
})

gg_miss_var_plot <- gg_miss_var(airquality)

test_that("gg_miss_var_works",{
  vdiffr::expect_doppelganger("gg_miss_var",
                              gg_miss_var_plot)
})

gg_miss_which_plot <- gg_miss_which(airquality)

test_that("gg_miss_which_works",{
  vdiffr::expect_doppelganger("gg_miss_which",
                              gg_miss_which_plot)
})

gg_miss_fct_plot <- gg_miss_fct(riskfactors, marital)

test_that("gg_miss_fct works",{
  vdiffr::expect_doppelganger("gg_miss_fct",
                              gg_miss_fct_plot)
})
