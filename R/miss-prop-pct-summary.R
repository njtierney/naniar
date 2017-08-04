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
#' @export
#'
#' @examples
#'
#' miss_prop_summary(airquality)
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
