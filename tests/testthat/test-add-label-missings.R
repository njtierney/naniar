context("add_label_missings")

# add some tests for this condition, where ... is added to label shadow
dat <- tibble::tribble(
  ~air, ~wind, ~water,
  -99,    NA,  23,
  -98,    NA,  NA,
  25,     30,  21,
  NA,     99,  NA,
  23,     40,  NA
)

test_that("add_label_missings returns a tibble",{
  expect_is(add_label_missings(dat), "tbl_df")
})

test_that("add_label_missings respects dimensions",{
  expect_equal(ncol(dat)+1, ncol(add_label_missings(dat)))
  expect_equal(nrow(dat), nrow(add_label_missings(dat)))
})

test_that("add_label_missings adds a column with suffix 'any_missing'",{
  expect_equal(names(add_label_missings(dat)),
               c(names(dat), "any_missing"))
})


dat_add <- add_label_missings(dat)
dat_add_air <- add_label_missings(dat, air)
dat_add_air_wind <- add_label_missings(dat, air, wind)

test_that("add_label_missings adds the right values to the column", {
  expect_equal(dat_add$any_missing,
               c("Missing", "Missing", "Not Missing", "Missing", "Missing"))
  expect_equal(dat_add_air$any_missing,
               c(rep("Not Missing", 3), "Missing", "Not Missing"))
})


dat_add_1 <- add_label_missings(dat, missing = "wat")
dat_add_2 <- add_label_missings(dat, complete = "even")
dat_add_3 <- add_label_missings(dat,
                                missing = "wat",
                                complete = "even")

test_that("add_label_shadow adds the right values to the column", {
  expect_equal(dat_add_1$any_missing,
               c("wat", "wat", "Not Missing", "wat", "wat"))
  expect_equal(dat_add_2$any_missing,
               c("Missing", "Missing", "even", "Missing", "Missing"))
  expect_equal(dat_add_3$any_missing,
               c("wat", "wat", "even", "wat", "wat"))
})

