#' Proportion of variables containing missings
#'
#' Calculate the proportion of variables that contain a single missing value.
#'
#' @param data a dataframe
#'
#' @return numeric the proportion of variables that contain missing data
#'
#' @export
#'
#' @examples
#'
#' na_var_prop(riskfactors)
#' na_var_prop(oceanbuoys)
#'
na_var_prop <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  # find the proportion of variables that contain (any) missing values
  mean(purrr::map_lgl(data, anyNA))


} # end function

#' Percentage of variables containing missings
#'
#' Calculate the percentage of variables that contain a single missing value.
#'
#' @param data a dataframe
#'
#' @return numeric the percent of variables that contain missing data
#'
#' @export
#'
#' @examples
#'
#' na_var_pct(riskfactors)
#' na_var_pct(oceanbuoys)
#'
na_var_pct <- function(data){

  # turn proportion into a percent
  na_var_prop(data) * 100

} # end function

#' Proportion of cases that contain a missing values.
#'
#' Calculate the proportion of cases (rows) that contain a missing value.
#'
#' @param data a dataframe
#'
#' @return numeric the proportion of cases that contain a missing value
#' @export
#'
#' @examples
#'
#' na_case_prop(airquality)
#'
na_case_prop <- function(data){

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

#' Percentage of cases that contain a missing values.
#'
#' Calculate the percentage of cases (rows) that contain a missing value.
#'
#' @param data a dataframe
#'
#' @return numeric the percentage of cases that contain a missing value
#' @export
#'
#' @examples
#'
#' na_case_pct(airquality)
#'
na_case_pct <- function(data){

  na_case_prop(data) * 100

}
