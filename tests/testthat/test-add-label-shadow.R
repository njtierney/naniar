context("add_label_shadow")
df <- data.frame(x = c(1:3),
                 y = c(NA, NA, 1))

df_sh <- nabular(df)

test_that("add_label_shadow returns a tibble",{
  expect_is(add_label_shadow(df_sh), "tbl_df")
  expect_is(add_label_shadow(df_sh, x), "tbl_df")
  expect_is(add_label_shadow(df_sh, x, y), "tbl_df")
})

test_that("add_label_shadow adds the right number of columns",{
  expect_equal(ncol(df_sh)+1, ncol(add_label_shadow(df_sh)))
  expect_equal(ncol(df_sh)+1, ncol(add_label_shadow(df_sh, x)))
  expect_equal(ncol(df_sh)+1, ncol(add_label_shadow(df_sh, x, y)))
})

test_that("add_label_shadow adds a column with suffix 'any_missing'",{
  expect_equal(names(add_label_shadow(df_sh)),
               c(names(df_sh), "any_missing"))
  expect_equal(names(add_label_shadow(df_sh, x)),
               c(names(df_sh), "any_missing"))
  expect_equal(names(add_label_shadow(df_sh, x, y)),
               c(names(df_sh), "any_missing"))
})

# add some tests for this condition, where ... is added to label shadow
dat <- tibble::tribble(
  ~air, ~wind, ~water,
   -99,    NA,  23,
   -98,    NA,  NA,
   25,     30,  21,
   NA,     99,  NA,
   23,     40,  NA
)

test_that("add_label_shadow errors when using a dataframe with no shadow", {
  expect_error(add_label_shadow(dat))
})


dat_sh <- nabular(dat)

dat_sh_add <- add_label_shadow(dat_sh)

dat_sh_add_air <- add_label_shadow(dat_sh, air)

dat_sh_add_air_wind <- add_label_shadow(dat_sh, air, wind)

test_that("add_label_shadow adds the right values to the column", {
  expect_equal(dat_sh_add$any_missing,
               c("Missing", "Missing", "Not Missing", "Missing", "Missing"))
  expect_equal(dat_sh_add_air$any_missing,
               c(rep("Not Missing", 3), "Missing", "Not Missing"))
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
  expect_equal(dat_sh_add_2$any_missing,
               c("Missing", "Missing", "even", "Missing", "Missing"))
  expect_equal(dat_sh_add_3$any_missing,
               c("wat", "wat", "even", "wat", "wat"))
})

