dat_ms <- tibble::tribble(~x,  ~y,    ~z,
                         1,   "A",   -100,
                         3,   "N/A", -99,
                         NA,  NA,    -98,
                         -99, "E",   -101,
                         -98, "F",   -1)
test_that("miss_scan_count returns a data.frame", {
  expect_s3_class(miss_scan_count(dat_ms, -99), "data.frame")
})

test_that("miss_scan_count returns an error when no search is provided", {
  expect_snapshot(
    error = TRUE,
    miss_scan_count(dat_ms)
  )
})

test_that("miss_scan_count returns an error when no data is provided", {
  expect_snapshot(
    error = TRUE,
    miss_scan_count(search = -99)
  )
})

test_that("miss_scan_count returns a data.frame of the right size", {
  expect_equal(
    dim(miss_scan_count(dat_ms, -99)),
    c(3, 3)
  )
})

test_that("miss_scan_count returns results sorted in descending order by 'n'", {
  result <- miss_scan_count(dat_ms, c(-99, -98))
  n_values <- result$n
  expect_true(all(n_values == sort(n_values, decreasing = TRUE)))
})

correct_answer_1 <- tibble::tribble(
  ~Variable, ~n, ~pct,
  "x", 1L, 20,
  "z", 1L, 20,
  "y", 0L, 0
)

correct_answer_2 <- tibble::tribble(
  ~Variable, ~n, ~pct,
  "x", 2L, 40,
  "z", 2L, 40,
  "y", 0L, 0
)


test_that("miss_scan_count returns the right answer", {
  expect_equal(miss_scan_count(dat_ms,-99),
               correct_answer_1)
  expect_equal(miss_scan_count(dat_ms,c(-99,-98)),
               correct_answer_2)
})



