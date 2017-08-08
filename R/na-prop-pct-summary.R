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
#' na_prop_summary(airquality)
#'
na_prop_summary <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("na_prop_summary")

}


#' @export
na_prop_summary.default <- function(data){

  tibble::tibble(df = prop_na(data),
                 var = na_var_prop(data),
                 case = na_case_prop(data))

}

#' @export
na_prop_summary.grouped_df <- function(data){

  group_by_fun(data, .fun = na_prop_summary)

}
