#' Proportions of missings in data, variables, and cases.
#'
#' Return missing data info about the dataframe, the variables, and the cases.
#'   Specifically, returning how many elements in a dataframe contain a missing
#'   value, how many elements in a variable contain a missing value, and how many
#'   elements in a case contain a missing.
#'
#' @param data a dataframe
#'
#' @return a dataframe
#'
#' @seealso [miss_case_pct]() [miss_case_prop]() [miss_case_summary]() [miss_case_table]() [miss_scan_count]() [miss_summary]() [miss_var_pct]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]()
#'
#' @export
#'
#' @examples
#'
#' miss_prop_summary(airquality)
#' library(dplyr)
#' airquality %>% group_by(Month) %>% miss_prop_summary()
#'
miss_prop_summary <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_prop_summary")

}


#' @export
miss_prop_summary.default <- function(data){

  tibble::tibble(df = prop_miss(data),
                 var = miss_var_prop(data),
                 case = miss_case_prop(data))

}

#' @export
miss_prop_summary.grouped_df <- function(data){

  group_by_fun(data, .fun = miss_prop_summary)

}
