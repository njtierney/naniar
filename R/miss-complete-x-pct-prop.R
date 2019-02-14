#' Proportion of variables containing missings or complete values
#'
#' Deprecated. Please see [miss_var_prop()] and [complete_var_prop()].
#'
#' @param data a dataframe
#'
#' @return numeric the proportion of variables that contain missing or complete
#'    data
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
#'
#' @export
#' @name miss-complete-var-prop
#'
miss_var_prop <- function(data){
  .Deprecated("prop_miss_var")
  test_if_null(data)

  test_if_dataframe(data)

  # find the proportion of variables that contain (any) missing values
  mean(purrr::map_lgl(data, anyNA))

} # end function

#' @export
#' @rdname miss-complete-var-prop
complete_var_prop <- function(data){
  .Deprecated("prop_complete_var")
  1 - miss_var_prop(data)

} # end function

#' Percentage of variables containing missings or complete values
#'
#' Deprecated. Please see [miss_var_pct()] and [complete_var_pct()].
#'
#' @param data a dataframe
#'
#' @return numeric the percent of variables that contain missing or complete
#'   data
#'
#' @export
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
#'
#'
#' @name miss-complete-var-pct
#'
miss_var_pct <- function(data){
  .Deprecated("pct_miss_var")
  # turn proportion into a percent
  miss_var_prop(data) * 100

} # end function

#' @export
#' @rdname miss-complete-var-pct

complete_var_pct <- function(data){
  .Deprecated("pct_complete_var")
  complete_var_prop(data) * 100

}

#' Proportion of cases that contain a missing or complete values.
#'
#' Deprecated, please see [miss_case_prop()] and [complete_case_prop()].
#'
#' @param data a dataframe
#'
#' @return numeric the proportion of cases that contain a missing or complete
#'     value
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
#'
#' @export
#' @name miss-complete-case-prop
#' @importFrom stats complete.cases
#'
miss_case_prop <- function(data){
  .Deprecated("prop_miss_case")
  test_if_null(data)

  test_if_dataframe(data)

  temp <- data %>%
    # which rows are complete?
    stats::complete.cases() %>%
    mean()

  # Return 1 if temp is 1
  # Prevent error when all the rows contain a NA and then mean is 1
  # so (1 -1)*100 = 0, whereas function should return 1
  if (temp == 1) {
    return(1)
  }

  if (temp == 0) {
    # Return 0 if temp is 0
    # Prevent error when no row contains a NA and then mean is 0
    # so (1 -0)*1 = 1, whereas function should return 0.
    return(0)
  }

  # return the output
  return((1 - temp))

}

#' @export
#' @rdname miss-complete-case-prop
complete_case_prop <- function(data){
  .Deprecated("prop_complete_case")
  1 - miss_case_prop(data)

}

#' Percentage of cases that contain a missing or complete values.
#'
#' Deprecated, please see [miss_case_pct()] and [complete_case_pct()].
#'
#' @param data a dataframe
#'
#' @return numeric the percentage of cases that contain a missing or complete
#'      value
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
#'
#' @export
#' @name miss-complete-case-pct
#'
miss_case_pct <- function(data){
  .Deprecated("pct_miss_case")
  miss_case_prop(data) * 100

}

#' @export
#' @rdname miss-complete-case-pct

complete_case_pct <- function(data){
  .Deprecated("pct_complete_case")
  complete_case_prop(data) * 100

}
