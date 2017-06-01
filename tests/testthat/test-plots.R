context("narnia-plots")

library(ggplot2)
geom_missing_point_plot <-
  ggplot(airquality,
         aes(x = Solar.R,
             y = Ozone)) +
  geom_missing_point()

test_that("geom_miss_point works",{
  vdiffr::expect_doppelganger("geom_missing_point",
                              geom_missing_point_plot)
})


gg_missing_case_plot <- gg_missing_case(airquality)

test_that("gg_missing_case_works",{
  vdiffr::expect_doppelganger("gg_missing_case",
                              gg_missing_case_plot)
})

gg_missing_var_plot <- gg_missing_var(airquality)

test_that("gg_missing_var_works",{
  vdiffr::expect_doppelganger("gg_missing_var",
                              gg_missing_var_plot)
})

gg_missing_which_plot <- gg_missing_which(airquality)

test_that("gg_missing_which_works",{
  vdiffr::expect_doppelganger("gg_missing_which",
                              gg_missing_which_plot)
})
