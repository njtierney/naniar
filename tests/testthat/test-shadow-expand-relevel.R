context("shadow_expand_relevel")

library(dplyr)

df_sh <- data.frame(Q1 = c("yes", "no", "no", NA),
                    Q2 = c("a", NA, NA, NA),
                    Q3 = c(1, NA, NA, 4)) %>%
  mutate(Q1 = factor(Q1)) %>%
  mutate(Q2 = factor(Q2)) %>%
  nabular()

test_that(desc = "when levels are repeated, they don't fail", {

  df_sh_recode_q2 <- df_sh %>%
    recode_shadow(Q2 = .where(Q1 %in% "no" ~ "skip"))

  expect_equal(levels(df_sh_recode_q2$Q2_NA),
               c("!NA",
                  "NA",
                  "NA_skip"))

  df_sh_recode_q3 <- df_sh_recode_q2 %>%
    recode_shadow(Q3 = .where(Q1 %in% "no" ~ "skip"))

  expect_equal(levels(df_sh_recode_q3$Q3_NA),
               c("!NA",
                  "NA",
                  "NA_skip"))

  q1_na_fct_vals <- df_sh_recode_q3$Q1_NA %>%
    table(., useNA = "always") %>%
    as.numeric()

  q2_na_fct_vals <- df_sh_recode_q3$Q2_NA %>%
    table(., useNA = "always") %>%
    as.numeric()

  q3_na_fct_vals <- df_sh_recode_q3$Q3_NA %>%
    table(., useNA = "always") %>%
    as.numeric()

  expect_equal(q1_na_fct_vals, c(3, 1, 0, 0))
  expect_equal(q2_na_fct_vals, c(1, 1, 2, 0))
  expect_equal(q3_na_fct_vals, c(2, 0, 2, 0))

})



