context("special missing values")

df <- tibble::tribble(
  ~wind, ~temp,
  -99,    45,
  68,    NA,
  72,    25
)

dfs <- bind_shadow(df)

dfs_special <- dfs %>%
  recode_shadow(temp = .where(wind == -99 ~ "bananas"))

test_that("special missings levels are updated", {
  expect_equal(levels(dfs_special$wind_NA),
               c("!NA", "NA", "NA_bananas"))
  expect_equal(levels(dfs_special$temp_NA),
              c("!NA", "NA", "NA_bananas"))
})

test_that("special missings are put in the right place", {
  expect_equal(as.character(dfs_special$wind_NA),
               c("!NA", "!NA", "!NA"))
  expect_equal(as.character(dfs_special$temp_NA),
              c("NA_bananas", "NA", "!NA"))
})

context("recode_shadow works for multiple variables")

# airquality %>%
#   bind_shadow() %>%
#   recode_shadow(Ozone = .where(Wind <= 5 ~ "broken_machine")) %>%
#   recode_shadow(Temp = .where(Temp <= 58 ~ "broken_temp")) %>%
#   arrange(Temp)
#
# aq_sh <- airquality %>%
#   bind_shadow() %>%
#   update_shadow("wat")
#
# aq_sh %>% map(levels)
#
# debugonce(update_shadow)
#
# aq_sh %>%
#   update_shadow("is") %>%
#   map(levels)
#
# # map(levels)

context("recode_shadow works on grouped data")

aq_recoded <- airquality %>%
  bind_shadow() %>%
  recode_shadow(Ozone = .where(Wind <= 5 ~ "broken_machine"))

aq_grouped_recoded <- airquality %>%
  bind_shadow() %>%
  dplyr::group_by(Month) %>%
  recode_shadow(Ozone = .where(Wind <= 5 ~ "broken_machine"))

test_that("special missings are the same for grouped and ungrouped data", {
  expect_equal(aq_grouped_recoded$Ozone_NA,
               aq_recoded$Ozone_NA)
})


# these are some old tests that explore how the `is_shadow` family work
#

#
# test_that("special missings return shadow", {
#   expect_true(is_shadow(dfs_special))
# })
#
# test_that("special missings are shadows", {
#   expect_true(is_shadow(dfs_special$wind_NA))
#   expect_true(is_shadow(dfs_special$temp_NA))
# })
#
# test_that("special missings return TRUE for any_shadow", {
#   expect_true(any_shadow(dfs_special))
# })
#
# are_dfs_special <- are_shadow(dfs_special)
#
# test_that("special missings return TRUE for are_shadow", {
#   expect_false(are_dfs_special[["wind"]])
#   expect_false(are_dfs_special[["temp"]])
#   expect_true(are_dfs_special[["wind_NA"]])
#   expect_true(are_dfs_special[["temp_NA"]])
# })
#
# test_that("special missings are shadows", {
#   expect_true(is_shadow(dfs_special$wind_NA))
#   expect_true(is_shadow(dfs_special$temp_NA))
# })
#
# test_shade <- dfs$wind_NA
#
# expanded_vec <- shadow_expand_relevel(test_shade, "weee")
#
# test_that("shadow_expand_relevel returns shadow type data",{
#   expect_is(expanded_vec,
#             "shadow")
# })
#
# releveled_df <- dplyr::mutate(dfs,
#                               temp_NA = shadow_expand_relevel(temp_NA, "weee"))
#
# test_that("shadow_expand_relevel returns shadows inside a data.frame", {
#   expect_is(releveled_df$wind, "numeric")
#   expect_is(releveled_df$temp, "numeric")
#   expect_is(releveled_df$wind_NA, "shadow")
#   expect_is(releveled_df$temp_NA, "shadow")
# })
