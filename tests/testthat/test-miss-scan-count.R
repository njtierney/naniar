context("miss-scan-count")

dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                         1,   "A",   -100,
                         3,   "N/A", -99,
                         NA,  NA,    -98,
                         -99, "E",   -101,
                         -98, "F",   -1)

test_that("miss_scan_count returns a data.frame", {
  expect_is(miss_scan_count(dat_ms,-99),
            "data.frame")
})

test_that("miss_scan_count returns an error when no search is provided", {
  expect_error(miss_scan_count(dat_ms))
})

test_that("miss_scan_count returns an error when no data is provided", {
  expect_error(miss_scan_count(search = -99))
})

test_that("miss_scan_count returns a data.frame of the right size", {
  expect_equal(dim(miss_scan_count(dat_ms,-99)),
                c(3,2))
})

correct_answer_1 <- tibble::tribble(
  ~Variable, ~n,
  "x", 1L,
  "y", 0L,
  "z", 1L
)

correct_answer_2 <- tibble::tribble(
  ~Variable, ~n,
  "x", 2L,
  "y", 0L,
  "z", 2L
)

correct_answer_3 <- tibble::tribble(
  ~Variable, ~n,
  "x", 2L,
  "y", 1L,
  "z", 2L
)

correct_answer_4 <- tibble::tribble(
  ~Variable, ~n,
  "x", 2L,
  "y", 1L,
  "z", 2L
)

test_that("miss_scan_count returns the right answer", {
  expect_equal(miss_scan_count(dat_ms,-99),
               correct_answer_1)
  expect_equal(miss_scan_count(dat_ms,c(-99,-98)),
               correct_answer_2)
})



