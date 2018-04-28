context("geom_miss_point")

library(ggplot2)
geom_miss_point_plot <- ggplot(airquality,
                               aes(x = Solar.R,
                                   y = Ozone)) +
  geom_miss_point()

test_that("geom_miss_point works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("geom_miss_point", geom_miss_point_plot)
})
