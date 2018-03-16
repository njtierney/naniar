# recode_shadow(.data,
#               x = where(x == -99 ~ "too_loud"),
#               y = where(x == -99 ~ "not_loud"))

# I see three steps to this process, kindly lifted from here:
# https://stackoverflow.com/questions/49214287/replace-nas-in-a-dataframe-with-factor-variables-with-r/49214861#49214861

# add the shadow level to all variables
  # new_level <- paste0("NA_", suffix)
  # levels(col) <- c(levels(col), new_level)
# change the order of the factors.
  # col <- relevel(col, ref = new_level)
# add the new level where appropriate
  # match_na_where(col) <- "new_level"
  # or some sort of tidy_eval thing.

# so, just to be clear, "update_shadow" will be run every time a new thing is added, and it will handle things so that the appropriate expansion and releveling is performed correctly

#' Expand and relevel a shadow column with a new suffix
#'
#' Internal function to handle appropriate expansion and releveling of
#'   shadow variables.
#'
#' @param .var a variable in a data.frame
#' @param suffix a character suffix to add to NA_, e.
#'
#' @return a factor with expanded levels
#'
#' @examples
#'
#' df <- tibble::tribble(
#'   ~wind, ~temp,
#'   -99,    45,
#'   68,    NA,
#'   72,    25
#' )
#'
#' dfs <- bind_shadow(df)
#'
#' test_shade <- dfs$wind_NA
#'
#' shadow_expand_relevel(test_shade, "weee")
#'
#' dfs %>%
#'   mutate(temp_NA = shadow_expand_relevel(temp_NA, "weee"))
#'
#' \dontrun{
#' # test that this breaks
#' shadow_expand_relevel(airquality, "weee")
#'}
shadow_expand_relevel <- function(.var, suffix){

  # is it a shadow?
  test_if_shadow(.var)

  # create level
  new_level <- paste0("NA_",suffix)

  # add the factor level
  new_var <- forcats::fct_expand(.var,levels(.var),new_level)
  # make sure that the order is preserved
  forcats::fct_relevel(new_var,levels(.var),new_level)
}


#' Expand all shadow levels
#'
#' Internal function to appropriately expand and relevel all shadow variables to include a new suffix
#'
#' @param data data.frame
#' @param suffix character vector
#'
#' @return data.frame with adjusted levels
#'
#' @examples
#'
#' df <- tibble::tribble(
#' ~wind, ~temp,
#' -99,    45,
#' 68,    NA,
#' 72,    25
#' )
#'
#'
#' dfs <- bind_shadow(df)
#'
#' update_shadow(dfs, "weee")
#' update_shadow(dfs, "weee") %>% what_levels()
#'
update_shadow <- function(data, suffix){
  dplyr::mutate_if(.tbl = data,
                   .predicate = is_shadow,
                   .funs = shadow_expand_relevel,
                   suffix = suffix)
}

# # recode_shadow(.data,
# #               x = where(x == -99 ~ "too_loud"),
# #               y = where(x == -99 ~ "not_loud"))
#
#
# recode_shadow <- function(data, ...){
#
#   expr <- rlang::quos(...)
#
#   print(expr)
#
#   print(names(expr))
#
#   print(quo_name(expr))
#
#   var_name_NA <- paste0(names(expr),"_NA")
#   var_name <- paste0(names(expr))
#
#   print(var_name_NA)
#   print(var_name)
#
#   data %>%
#     dplyr::mutate(
#       !!var_name_NA := forcats::fct_expand(!!var_name_NA, "NA_suffix"),
#       !!var_name_NA := dplyr::case_when(
#         !!var_name == -99 ~ factor("NA_suffix",
#                                    levels = c("!NA", "NA", "NA_suffix")),
#         TRUE ~ !!var_name
#       ))
#
# }
#
