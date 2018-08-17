context("add_label_shadow")

aq_shadow <- airquality %>% add_shadow(Ozone)

test_that("add_label_shadow returns a tibble",{
  expect_is(add_label_shadow(aq_shadow), "tbl_df")
})

test_that("add_label_shadow adds the right number of columns",{
  expect_equal(ncol(aq_shadow)+1, ncol(add_label_shadow(aq_shadow)))
})

test_that("add_label_shadow adds a column with suffix 'any_missing'",{
  expect_equal(names(add_label_shadow(aq_shadow)),
               c(names(aq_shadow), "any_missing"))
})

# add some tests for this condition, where ... is added to label shadow
# aq_shade <- bind_shadow(airquality)
#
# # debug(label_shadow)
#
# add_label_shadow(aq_shade,
#                  Ozone) %>%
#   dplyr::select(Ozone,
#                 Solar.R,
#                 Ozone_NA,
#                 Solar.R_NA,
#                 any_missing)
#
# add_label_shadow(aq_shade) %>%
#   dplyr::select(Ozone,
#                 Solar.R,
#                 Ozone_NA,
#                 Solar.R_NA,
#                 any_missing)
