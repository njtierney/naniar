context("gg_miss_which")

dat <- tibble::tribble(
  ~air, ~wind, ~water, ~month,
  -99,    NA,  23,     1,
  -98,    NA,  NA,     1,
  25,     30,  21,     2,
  NA,     99,  NA,     2,
  23,     40,  NA,     2
)

gg_miss_which_plot <- gg_miss_which(dat)

test_that("gg_miss_which_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_which",
                              gg_miss_which_plot)
})
