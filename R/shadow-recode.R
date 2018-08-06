# recode_shadow(.data,
#               x = .where(x == -99 ~ "too_loud"),
#               y = .where(x == -99 ~ "not_loud"))

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
#' \dontrun{
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
#'
#' # test that this breaks
#' shadow_expand_relevel(airquality, "weee")
#'}
shadow_expand_relevel <- function(.var, suffix){

  # is it a shadow?
  # test_if_shadow(.var)
  # - no longer needed, as mutate_if tests the predicate
  #  -asking "is this a shadow" with is_shadow

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
#'  \dontrun{
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
#' }
#'
update_shadow <- function(data, suffix){
  dplyr::mutate_if(.tbl = data,
                   .predicate = is_shadow,
                   .funs = shadow_expand_relevel,
                   suffix = suffix)
}

#' split a call into two components with a useful verb name
#'
#' This function is used inside `recode_shadow` to help evaluate the formula
#'   call effectively. `.where` is a little special - you shouldn't use it outside the function `recode_shadow`.
#'
#' @param ... case_when formula
#'
#' @return a list of "condition" and "suffix" arguments
#' @name where
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df <- tibble::tribble(
#' ~wind, ~temp,
#' -99,    45,
#' 68,    NA,
#' 72,    25
#' )
#'
#' dfs <- bind_shadow(df)
#'
#' recode_shadow(dfs,
#'               temp = .where(wind == -99 ~ "bananas"))
#'
#' }
#'
.where <- function(...){
  formulas <- rlang::dots_list(...)

  fun_rhs <- rlang::f_rhs(formulas[[1]])
  fun_lhs <- rlang::f_lhs(formulas[[1]])

  list(condition = fun_lhs,
       suffix = fun_rhs)

}

#' Add special missing values to the shadow matrix
#'
#' It can be useful to add special missing values, naniar supports this with
#'   the `recode_shadow` function.
#'
#' @note This only works for one special missing at a time at the moment.
#'
#' @param data data.frame
#' @param ... A sequence of two-sided formulas as in dplyr::case_when,
#'   but when a wrapper function `.where` written around it.
#'
#' @return a dataframe with altered shadows
#' @export
#'
#' @examples
#'
#' \dontrun{
#' df <- tibble::tribble(
#' ~wind, ~temp,
#' -99,    45,
#' 68,    NA,
#' 72,    25
#' )
#'
#' dfs <- bind_shadow(df)
#'
#' dfs
#'
#' recode_shadow(dfs,
#'               temp = .where(wind == -99 ~ "bananas"))
#'
#' # need to debug this
#'
#' recode_shadow(dfs,
#'               temp = .where(wind == -99 ~ "bananas")) %>%
#' recode_shadow(wind = .where(wind == -99 ~ "my_shhh_is"))
#' }
#'
recode_shadow <- function(data, ...){
  quo_var <- rlang::quos(...)

  formulas <- rlang::dots_list(...)

  formulas_pluck <- purrr::pluck(formulas, 1)

  condition <- purrr::pluck(formulas_pluck, "condition")

  suffix <- purrr::pluck(formulas_pluck, "suffix")

  na_suffix <- paste0("NA_", suffix)

  shadow_var <- rlang::sym(paste0(names(quo_var),"_NA"))

  data %>%
    update_shadow(suffix) %>%
    dplyr::mutate(
      !!shadow_var := dplyr::case_when(
        !!condition ~ factor(na_suffix,
                           levels = levels(.[[shadow_var]])),
        TRUE ~ !!shadow_var
      ))
}
