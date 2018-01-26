context("gg_miss_span")

gg_miss_span_plot <- gg_miss_span(pedestrian, hourly_counts, 4000)

test_that("gg_miss_span works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_span",
                              gg_miss_span_plot)

})

gg_miss_span_plot_group <- gg_miss_span(pedestrian,
                                        hourly_counts,
                                        4000,
                                        group = sensor_name)

test_that("gg_miss_span_group works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_span_group",
                              gg_miss_span_plot_group)
})

