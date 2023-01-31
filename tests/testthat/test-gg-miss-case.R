# gg_miss_case plots -----------------------------------------------------------

dat <- tibble::tribble(
  ~air, ~wind, ~water, ~month,
  -99,    NA,  23,     1,
  -98,    NA,  NA,     1,
  25,     30,  21,     2,
  NA,     99,  NA,     2,
  23,     40,  NA,     2
)

gg_miss_case_plot <- gg_miss_case(dat)

test_that("gg_miss_case_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_case",
                              gg_miss_case_plot)
})

gg_miss_case_plot_group <- gg_miss_case(dat, facet = month)

test_that("gg_miss_case_group_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_case_group",
                              gg_miss_case_plot_group)
})

gg_miss_case_plot_sort <- gg_miss_case(dat, order_cases = TRUE)

test_that("gg_miss_case_sort_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_case_sort",
                              gg_miss_case_plot_sort)
})

gg_miss_case_plot_order_group_sort <- gg_miss_case(dat,
                                                   facet = month,
                                                   order_cases = TRUE)

test_that("gg_miss_case_group_and_sort_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_case_group_and_sort",
                              gg_miss_case_plot_order_group_sort)
})

gg_miss_case_plot_show_pct <- gg_miss_case(dat, show_pct = TRUE)

test_that("gg_miss_case_show_pct_works",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_case_plot_show_pct",
                              gg_miss_case_plot_show_pct)
})

gg_miss_case_plot_group_show_pct <- gg_miss_case(dat,
                                                 facet = month,
                                                 show_pct = TRUE)

test_that("gg_miss_case_group_works_show_pct",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_case_group_show_pct",
                              gg_miss_case_plot_group_show_pct)
})

gg_miss_case_plot_sort_show_pct <- gg_miss_case(dat,
                                                order_cases = TRUE,
                                                show_pct = TRUE)

test_that("gg_miss_case_sort_works_show_pct",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_case_sort_show_pct",
                              gg_miss_case_plot_sort_show_pct)
})

gg_miss_case_plot_order_group_sort_show_pct <- gg_miss_case(dat,
                                                            facet = month,
                                                            order_cases = TRUE,
                                                            show_pct = TRUE)

test_that("gg_miss_case_group_and_sort_works_show_pct",{
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("gg_miss_case_group_and_sort_show_pct",
                              gg_miss_case_plot_order_group_sort_show_pct)
})
