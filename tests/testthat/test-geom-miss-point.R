context("geom_miss_point")

library(ggplot2)
geom_miss_point_plot <- ggplot(airquality,
                               aes(x = Solar.R,
                                   y = Ozone)) +
  geom_miss_point()

test_that("geom_miss_point works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("geom_miss_point", geom_miss_point_plot)
})

geom_miss_point_plot_jitter <-
  ggplot(airquality,
         aes(x = Solar.R,
             y = Ozone)) +
  geom_miss_point(jitter = 0.5)

test_that("geom_miss_point_jitter works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("geom_miss_point_jitter",
                              geom_miss_point_plot_jitter)
})

geom_miss_point_plot_prop <-
  ggplot(airquality,
         aes(x = Solar.R,
             y = Ozone)) +
  geom_miss_point(prop_below = 0.5)

test_that("geom_miss_point_prop below works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("geom_miss_point_prop",
                              geom_miss_point_plot_prop)
})

geom_miss_point_plot_prop_jitter <-
  ggplot(airquality,
         aes(x = Solar.R,
             y = Ozone)) +
  geom_miss_point(prop_below = 0.5,
                  jitter = 0.5)

test_that("geom_miss_point_prop_jitter works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("geom_miss_point_prop_jitter",
                              geom_miss_point_plot_prop_jitter)
})
