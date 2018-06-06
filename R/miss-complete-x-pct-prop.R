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
#' @seealso [miss_case_pct]() [miss_case_prop]() [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_pct]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]()
#'
#' @export
#' @name miss-complete-var-prop
#'
#' @examples
#'
#' miss_var_prop(riskfactors)
#' miss_var_prop(oceanbuoys)
#' complete_var_prop(riskfactors)
#' complete_var_prop(oceanbuoys)
#'
miss_var_prop <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  # find the proportion of variables that contain (any) missing values
  mean(purrr::map_lgl(data, anyNA))

} # end function

#' @export
#' @rdname miss-complete-var-prop
complete_var_prop <- function(data){

  1 - miss_var_prop(data)

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
#' @seealso [miss_case_pct]() [miss_case_prop]() [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]()
#'
#'
#' @name miss-complete-var-pct
#' @examples
#'
#' miss_var_pct(riskfactors)
#' miss_var_pct(oceanbuoys)
#' complete_var_pct(riskfactors)
#' complete_var_pct(oceanbuoys)
#'
miss_var_pct <- function(data){

  # turn proportion into a percent
  miss_var_prop(data) * 100

} # end function

#' @export
#' @rdname miss-complete-var-pct

complete_var_pct <- function(data){

  complete_var_prop(data) * 100

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
#' @seealso [miss_case_pct]() [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_pct]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]()
#'
#' @export
#' @name miss-complete-case-prop
#' @importFrom stats complete.cases
#' @examples
#'
#' miss_case_prop(airquality)
#' complete_case_prop(airquality)
#'
miss_case_prop <- function(data){

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
  } else if (temp == 0) {
    # Return 0 if temp is 0
    # Prevent error when no row contains a NA and then mean is 0
    # so (1 -0)*1 = 1, whereas function should return 0.
    return(0)
  } else {
    return((1 - temp))
  }

}

#' @export
#' @rdname miss-complete-case-prop
complete_case_prop <- function(data){

  1 - miss_case_prop(data)

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
#' @seealso [miss_case_prop]() [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_pct]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]()
#'
#' @export
#' @name miss-complete-case-pct
#'
#' @examples
#'
#' miss_case_pct(airquality)
#' complete_case_pct(airquality)
#'
miss_case_pct <- function(data){

  miss_case_prop(data) * 100

}

#' @export
#' @rdname miss-complete-case-pct

complete_case_pct <- function(data){

  complete_case_prop(data) * 100

}
