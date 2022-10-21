# add some tests for this condition, where ... is added to label shadow
dat <- tibble::tribble(
  ~air, ~wind, ~water,
  -99,    NA,  23,
  -98,    NA,  NA,
  25,     30,  21,
  NA,     99,  NA,
  23,     40,  NA
)

dat_sh <- nabular(dat)

test_that("add_label_shadow returns a tibble",{
  expect_is(add_label_shadow(dat_sh), "tbl_df")
  expect_is(add_label_shadow(dat_sh, air, wind), "tbl_df")
})

test_that("add_label_shadow adds the right number of columns",{
  expect_equal(ncol(dat_sh)+1, ncol(add_label_shadow(dat_sh)))
  expect_equal(ncol(dat_sh)+1, ncol(add_label_shadow(dat_sh, air, wind)))
})

test_that("add_label_shadow adds a column with suffix 'any_missing'",{
  expect_equal(names(add_label_shadow(dat_sh)),
               c(names(dat_sh), "any_missing"))
  expect_equal(names(add_label_shadow(dat_sh, air, wind)),
               c(names(dat_sh), "any_missing"))
})

test_that("add_label_shadow errors when using a dataframe with no shadow", {
  expect_error(add_label_shadow(dat))
})

dat_sh_add <- add_label_shadow(dat_sh)
dat_sh_add_air_wind <- add_label_shadow(dat_sh, air, wind)

test_that("add_label_shadow adds the right values to the column", {
  expect_equal(dat_sh_add$any_missing,
               c("Missing", "Missing", "Not Missing", "Missing", "Missing"))
  expect_equal(dat_sh_add_air_wind$any_missing,
               c(rep("Missing", 2), "Not Missing", "Missing", "Not Missing"))
})

dat_sh_add_1 <- add_label_shadow(dat_sh,
                                 missing = "wat")
dat_sh_add_2 <- add_label_shadow(dat_sh,
                                 complete = "even")
dat_sh_add_3 <- add_label_shadow(dat_sh,
                                 missing = "wat",
                                 complete = "even")


test_that("add_label_shadow adds the right values to the column", {
  expect_equal(dat_sh_add_1$any_missing,
               c("wat", "wat", "Not Missing", "wat", "wat"))
  expect_equal(dat_sh_add_3$any_missing,
               c("wat", "wat", "even", "wat", "wat"))
})

