df <- tibble::tribble(
  ~wind, ~temp,
  -99,    45,
  68,    NA,
  72,    25
)

test_that("recode_shadow errors when regular dataframe passed",{
  expect_error(recode_shadow(df, temp = .where(wind == -99 ~ "bananas")))
})

dfs <- nabular(df)

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

df_many_recode <- df %>%
  nabular() %>%
  recode_shadow(temp = .where(temp == 25 ~ "broken_machine")) %>%
  recode_shadow(wind = .where(wind == -99 ~ "broken_temp"))

new_shade_levels <- c("!NA", "NA", "NA_broken_machine", "NA_broken_temp")

test_that("special missings levels are updated for many recodes", {
  skip_on_cran()
  expect_equal(levels(df_many_recode$wind_NA),
               new_shade_levels)
  expect_equal(levels(df_many_recode$temp_NA),
               new_shade_levels)
})

test_that("special missings are put in the right place for many recodes", {
  skip_on_cran()
  expect_equal(as.character(df_many_recode$wind_NA),
               c("NA_broken_temp", "!NA", "!NA"))
  expect_equal(as.character(df_many_recode$temp_NA),
               c("!NA", "NA", "NA_broken_machine"))
})

where_one <- .where(temp == 25 ~ "broken_machine")

where_two <- .where(temp == 25 ~ "broken_machine",
                    wind == 26 ~ "broken_wind")

where_three <- .where(temp == 25 ~ "broken_machine",
                      wind == 26 ~ "broken_wind",
                      arbitrary == "values" ~ "are_possible")

test_that(".where captures the right number of expressions", {
  expect_equal(sum(lengths(where_one)), 2)
  expect_equal(sum(lengths(where_two)), 4)
  expect_equal(sum(lengths(where_three)), 6)
})

test_that(".where captures the expressions into condition and suffix", {
  expect_equal(names(where_one), c("condition", "suffix"))
  expect_equal(names(where_two), c("condition", "suffix"))
  expect_equal(names(where_three), c("condition", "suffix"))
})

test_that(".where is a list", {
  expect_is(where_one, "list")
  expect_is(where_two, "list")
  expect_is(where_three, "list")
})

test_that(".where is a list", {
  expect_is(where_one, "list")
  expect_is(where_two, "list")
  expect_is(where_three, "list")
})

class_nest <- function(obj, slot) {
  purrr::pluck(obj, slot) %>% purrr::map_chr(class)
}

test_that(".where returns call class", {
  expect_true(class_nest(where_one, "condition") == "call")
  expect_true(all(class_nest(where_two, "condition") == "call"))
  expect_true(all(class_nest(where_three, "condition") == "call"))
})

test_that(".where returns chr class", {
  expect_true(class_nest(where_one, "suffix") == "character")
  expect_true(all(class_nest(where_two, "suffix") == "character"))
  expect_true(all(class_nest(where_three, "suffix") == "character"))
})

df_mult_where <- df %>%
  nabular() %>%
  recode_shadow(temp = .where(temp == 25 ~ "broken_machine",
                              wind == 68 ~ "wat"))

test_that("recode_shadow returns right values if .where is called many times", {
  expect_equal(as.character(df_mult_where$wind_NA),
               c("!NA", "!NA", "!NA"))
  expect_equal(as.character(df_mult_where$temp_NA),
               c("!NA", "NA_wat", "NA_broken_machine"))
})

test_that("recode_shadow returns right levels if .where is called many times", {
  expect_equal(levels(df_mult_where$wind_NA),
               c("!NA", "NA", "NA_broken_machine", "NA_wat"))
  expect_equal(levels(df_mult_where$temp_NA),
               c("!NA", "NA", "NA_broken_machine", "NA_wat"))
})

aq_recoded <- airquality %>%
  nabular() %>%
  recode_shadow(Ozone = .where(Wind <= 5 ~ "broken_machine"))

aq_grouped_recoded <- airquality %>%
  nabular() %>%
  dplyr::group_by(Month) %>%
  recode_shadow(Ozone = .where(Wind <= 5 ~ "broken_machine"))

test_that("special missings are the same for grouped and ungrouped data", {
  expect_equal(as.character(aq_grouped_recoded$Ozone_NA),
               as.character(aq_recoded$Ozone_NA))
})


test_that("special missings class is maintained for grouped and ungrouped data", {
  skip_on_cran()
  skip_on_ci()
  expect_equal(class(aq_grouped_recoded$Ozone_NA),
               class(aq_recoded$Ozone_NA))
  expect_true(is_shade(aq_grouped_recoded$Ozone_NA),
              is_shade(aq_recoded$Ozone_NA))
})

test_that("shadow_expand_relevel returns shadows inside a data.frame", {
  expect_is(dfs_special$wind, "numeric")
  expect_is(dfs_special$temp, "numeric")
  expect_is(dfs_special$wind_NA, "shade")
  expect_is(dfs_special$temp_NA, "shade")
})
