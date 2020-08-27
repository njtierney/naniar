#' Proportion of variables containing missings or complete values
#'
#' Calculate the proportion of variables that contain a single missing or
#'    complete values.
#'
#' @param data a dataframe
#'
#' @return numeric the proportion of variables that contain missing or complete
#'    data
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
#'
#' @export
#' @name prop-miss-complete-var
#'
#' @examples
#'
#' prop_miss_var(airquality)
#' prop_complete_var(airquality)
#'
prop_miss_var <- function(data){
  test_if_null(data)

  test_if_dataframe(data)

  # find the proportion of variables that contain (any) missing values
  mean(colSums(is.na(data)) > 0)

} # end function

#' @export
#' @rdname prop-miss-complete-var
prop_complete_var <- function(data){
  1 - prop_miss_var(data)

} # end function

#' Percentage of variables containing missings or complete values
#'
#' Calculate the percentage of variables that contain a single missing or
#'     complete value.
#'
#' @param data a dataframe
#'
#' @return numeric the percent of variables that contain missing or complete
#'   data
#'
#' @export
#'
#' @seealso [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
#'
#'
#' @name pct-miss-complete-var
#' @examples
#'
#' prop_miss_var(airquality)
#' prop_complete_var(airquality)
#'
pct_miss_var <- function(data){
  # turn proportion into a percent
  prop_miss_var(data) * 100

} # end function

#' @export
#' @rdname pct-miss-complete-var

pct_complete_var <- function(data){
  prop_complete_var(data) * 100
}

#' Proportion of cases that contain a missing or complete values.
#'
#' Calculate the proportion of cases (rows) that contain missing or complete
#'     values.
#'
#' @param data a dataframe
#'
#' @return numeric the proportion of cases that contain a missing or complete
#'     value
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
#'
#' @export
#' @name prop-miss-complete-case
#' @importFrom stats complete.cases
#' @examples
#'
#' prop_miss_case(airquality)
#' prop_complete_case(airquality)
#'
prop_miss_case <- function(data){
  test_if_null(data)
  test_if_dataframe(data)

  # How many missings in each row?
  n_miss_in_rows <- rowSums(is.na(data))

  # What is the proportion of rows with any missings?
  mean(n_miss_in_rows > 0)
}

#' @export
#' @rdname prop-miss-complete-case
prop_complete_case <- function(data){
  1 - prop_miss_case(data)
}

#' Percentage of cases that contain a missing or complete values.
#'
#' Calculate the percentage of cases (rows) that contain a missing or complete
#'    value.
#'
#' @param data a dataframe
#'
#' @return numeric the percentage of cases that contain a missing or complete
#'      value
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
#'
#' @export
#' @name pct-miss-complete-case
#'
#' @examples
#'
#' pct_miss_case(airquality)
#' pct_complete_case(airquality)
#'
pct_miss_case <- function(data){
  prop_miss_case(data) * 100
}

#' @export
#' @rdname pct-miss-complete-case

pct_complete_case <- function(data){
  prop_complete_case(data) * 100

}
