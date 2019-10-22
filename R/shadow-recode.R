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

  # create level
  new_level <- glue::glue("NA_{suffix}")

  # add the factor level
  new_var <- forcats::fct_expand(f = .var,
                                 levels(.var),
                                 new_level)

  new_var <- forcats::fct_relevel(new_var,
                                      levels(.var),
                                      new_level)

  return(new_var)

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
update_shadow <- function(data, suffix) {

  class_of_cols <- purrr::map(data,class)
  attributes_of_data <- attributes(data)

  updated_shadow <-
  dplyr::mutate_if(.tbl = data,
                   .predicate = is_shade,
                   .funs = shadow_expand_relevel,
                   suffix = suffix)

  # write a function to assist in preserving the class of the data
  # and the columns
  updated_shadow <- purrr::map2_dfc(updated_shadow,
                                    class_of_cols,
                                    `class<-`)

  attributes(updated_shadow) <- attributes_of_data

  updated_shadow

}

#' Split a call into two components with a useful verb name
#'
#' This function is used inside `recode_shadow` to help evaluate the formula
#'   call effectively. `.where` is a special function designed for use in
#'   `recode_shadow`, and you shouldn't use it outside of it
#'
#' @param ... case_when style formula
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

  fun_lhs <- purrr::map(formulas, f_lhs)
  fun_rhs <- purrr::map(formulas, f_rhs)

  list(condition = fun_lhs,
       suffix = fun_rhs)
}

#' Add special missing values to the shadow matrix
#'
#' It can be useful to add special missing values, naniar supports this with
#'   the `recode_shadow` function.
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
#' recode_shadow(dfs, temp = .where(wind == -99 ~ "bananas"))
#'
#' # need to debug this
#'
#' recode_shadow(dfs,
#'               temp = .where(wind == -99 ~ "bananas")) %>%
#' recode_shadow(wind = .where(wind == -99 ~ "apples"))
#' }
#'
recode_shadow <- function(data, ...){

  test_if_null(data)
  test_if_any_shade(data)

  formulas <- rlang::dots_list(...)

  condition <- formulas %>% purrr::map("condition")

  suffix <- formulas %>% purrr::map("suffix")

  na_suffix <- purrr::map(suffix, ~ glue::glue("NA_{.x}"))

  shadow_var <- rlang::syms(glue::glue("{names(formulas)}_NA"))

  # build up the expressions to pass to case_when
  magic_shade_exprs <- purrr::pmap(
    .l = list(condition,
              na_suffix,
              shadow_var),
    .f =  function(condition,
                   na_suffix,
                   shadow_var){
      purrr::map2(
        condition,
        na_suffix,
        function(condition,
                 na_suffix){
          rlang::expr(
            !!condition ~ factor(!!na_suffix,
                                 levels = levels(.[[!!as_string(shadow_var)]]))
                 )
              }
         )
       })

  # evaluate the cases in case_when and ensure that the case_when
  # keeps everything as a shade/factor
  magic_shade_case_when <- magic_shade_exprs %>%
    purrr::map2(
      shadow_var,
      function(cases,
               shadow_var){
        rlang::expr(
          structure(
            dplyr::case_when(
              !!!cases,
              TRUE ~ factor(!!shadow_var,
                            levels = levels(.[[!!as_string(shadow_var)]]))
              ),
            class = c("shade", "factor")
            )
          )
        }) %>%
    rlang::set_names(purrr::map_chr(shadow_var, as_string))

  shadow_recoded <- data %>%
    update_shadow(unlist(suffix, use.names = FALSE)) %>%
    dplyr::mutate(!!!magic_shade_case_when)

  shadow_recoded
}
