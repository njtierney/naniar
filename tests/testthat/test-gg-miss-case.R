# gg_miss_case plots -----------------------------------------------------------

context("gg_miss_case")

gg_miss_case_plot <- gg_miss_case(airquality)

test_that("gg_miss_case_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case",
                              gg_miss_case_plot)
})

gg_miss_case_plot_group <- gg_miss_case(airquality, facet = Month)

test_that("gg_miss_case_group_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case_group",
                              gg_miss_case_plot_group)
})

gg_miss_case_plot_sort <- gg_miss_case(airquality, order_cases = TRUE)

test_that("gg_miss_case_sort_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case_sort",
                              gg_miss_case_plot_sort)
})

gg_miss_case_plot_order_group_sort <- gg_miss_case(airquality,
                                                   facet = Month,
                                                   order_cases = TRUE)

test_that("gg_miss_case_group_and_sort_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case_group_and_sort",
                              gg_miss_case_plot_order_group_sort)
})

gg_miss_case_plot_show_pct <- gg_miss_case(airquality, show_pct = TRUE)

test_that("gg_miss_case_show_pct_works",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case_plot_show_pct",
                              gg_miss_case_plot_show_pct)
})

gg_miss_case_plot_group_show_pct <- gg_miss_case(airquality,
                                                 facet = Month,
                                                 show_pct = TRUE)

test_that("gg_miss_case_group_works_show_pct",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case_group_show_pct",
                              gg_miss_case_plot_group_show_pct)
})

gg_miss_case_plot_sort_show_pct <- gg_miss_case(airquality,
                                                order_cases = TRUE,
                                                show_pct = TRUE)

test_that("gg_miss_case_sort_works_show_pct",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case_sort_show_pct",
                              gg_miss_case_plot_sort_show_pct)
})

gg_miss_case_plot_order_group_sort_show_pct <- gg_miss_case(airquality,
                                                            facet = Month,
                                                            order_cases = TRUE,
                                                            show_pct = TRUE)

test_that("gg_miss_case_group_and_sort_works_show_pct",{
  skip_on_cran()
  skip_on_appveyor()
  vdiffr::expect_doppelganger("gg_miss_case_group_and_sort_show_pct",
                              gg_miss_case_plot_order_group_sort_show_pct)
})
