dat <- tibble::tribble(
  ~air, ~wind, ~water, ~month,
  -99,    NA,  23,     1,
  -98,    NA,  NA,     1,
  25,     30,  21,     2,
  NA,     99,  NA,     2,
  23,     40,  NA,     2
)

# gg_miss_span_plot <- gg_miss_span(pedestrian, hourly_counts, 4000)
gg_miss_span_plot <- gg_miss_span(dat, water, 3)

test_that("gg_miss_span works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_span",
                              gg_miss_span_plot)

})

gg_miss_span_plot_group <- gg_miss_span(dat,
                                        water,
                                        3,
                                        facet = month)

test_that("gg_miss_span_group works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_span_group",
                              gg_miss_span_plot_group)
})

