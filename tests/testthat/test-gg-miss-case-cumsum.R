# gg_miss_case_cumsum plots ----------------------------------------------------

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

gg_miss_case_cumsum_plot <- gg_miss_case_cumsum(dat)

test_that("gg_miss_case_cumsum_works", {
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_case_cumsum", gg_miss_case_cumsum_plot)
})
