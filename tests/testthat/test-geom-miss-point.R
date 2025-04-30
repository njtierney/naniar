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

library(ggplot2)
geom_miss_point_plot <- ggplot(dat, aes(x = wind, y = water)) +
  geom_miss_point()

test_that("geom_miss_point works", {
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("geom_miss_point", geom_miss_point_plot)
})

geom_miss_point_plot_jitter <-
  ggplot(dat, aes(x = wind, y = water)) +
  geom_miss_point(jitter = 0.5)

test_that("geom_miss_point_jitter works", {
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger(
    "geom_miss_point_jitter",
    geom_miss_point_plot_jitter
  )
})

geom_miss_point_plot_prop <-
  ggplot(dat, aes(x = wind, y = water)) +
  geom_miss_point(prop_below = 0.5)

test_that("geom_miss_point_prop below works", {
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger("geom_miss_point_prop", geom_miss_point_plot_prop)
})

geom_miss_point_plot_prop_jitter <-
  ggplot(dat, aes(x = wind, y = water)) +
  geom_miss_point(prop_below = 0.5, jitter = 0.5)

test_that("geom_miss_point_prop_jitter works", {
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger(
    "geom_miss_point_prop_jitter",
    geom_miss_point_plot_prop_jitter
  )
})

dat_date <- tibble::tibble(
  values = 1:7,
  number = c(111, 112, NA, NA, 108, 150, 160),
  posixct = as.POSIXct(number, origin = "1970-01-01"),
  posixlt = as.POSIXlt(number, origin = "1970-01-01"),
  date = as.Date(number)
)

geom_miss_point_plot_date <- ggplot(dat_date, aes(x = values, y = date)) +
  geom_miss_point()

test_that("geom_miss_point works with Dates", {
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger(
    "geom_miss_point_plot_date",
    geom_miss_point_plot_date
  )
})


geom_miss_point_plot_date_prop_jitter <-
  ggplot(dat_date, aes(x = values, y = date)) +
  geom_miss_point(prop_below = 0.5, jitter = 0.5)

test_that("geom_miss_point_prop_jitter works", {
  skip_on_cran()
  skip_on_ci()
  vdiffr::expect_doppelganger(
    "geom_miss_point_plot_date_prop_jitter",
    geom_miss_point_plot_date_prop_jitter
  )
})
