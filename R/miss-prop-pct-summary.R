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
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
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
                 var = prop_miss_var(data),
                 case = prop_miss_case(data))

}

#' @export
miss_prop_summary.grouped_df <- function(data){

  group_by_fun(data, .fun = miss_prop_summary)

}
