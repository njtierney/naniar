dat <- tibble::tribble(
  ~air,
  ~wind,
  ~water,
  ~month,
  -99,
  NA,
  23,
  1,
  -98,
  NA,
  NA,
  1,
  25,
  30,
  21,
  2,
  NA,
  99,
  NA,
  2,
  23,
  40,
  NA,
  2
)

gg_miss_fct_plot <- gg_miss_fct(dat, month)

test_that("gg_miss_fct works", {
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_fct", gg_miss_fct_plot)
})
