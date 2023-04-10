#' Cumulative sum of the number of missings in each variable
#'
#' Calculate the cumulative sum of number & percentage of
#'   missingness for each variable.
#'
#' @param data a data.frame
#'
#' @return a tibble of the cumulative sum of missing data in each variable
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
#'
#' @examples
#'
#' miss_var_cumsum(airquality)
#' \dontrun{
#' library(dplyr)
#'
#' # respects dplyr::group_by
#'
#' airquality %>%
#'   group_by(Month) %>%
#'   miss_var_cumsum()
#'}
#' @export
miss_var_cumsum <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_var_cumsum")

}

#' @export
miss_var_cumsum.default <- function(data){

  purrr::map_dfc(data,
                # how many are missing in each variable?
                function(x) n_miss(x)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "variable",
                        values_to = "n_miss") %>%
    dplyr::mutate(n_miss_cumsum = cumsum(n_miss))

}

#' @export
miss_var_cumsum.grouped_df <- function(data){

  group_by_fun(data, .fun = miss_var_cumsum)

}


#' Summarise the missingness in each case
#'
#' Provide a data.frame containing each case (row), the number and percent of
#'   missing values in each case.
#'
#' @param data a dataframe
#'
#' @return a tibble containing the number and percent of missing data in each
#'   case
#'
#' @examples
#'
#' miss_case_cumsum(airquality)
#'
#'\dontrun{
#' library(dplyr)
#'
#' airquality %>%
#'   group_by(Month) %>%
#'   miss_case_cumsum()
#'}
#' @export
miss_case_cumsum <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_case_cumsum")
}

#' @export
miss_case_cumsum.default <- function(data){

  miss_case_summary(data) %>%
    dplyr::arrange(case) %>%
    dplyr::select(case,
                  n_miss) %>%
    dplyr::mutate(n_miss_cumsum = cumsum(n_miss))
}

#' @export
miss_case_cumsum.grouped_df <- function(data){

  group_by_fun(data, .fun = miss_case_cumsum)

}
