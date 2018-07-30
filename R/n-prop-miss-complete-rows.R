#' Return a vector of the number of missing values in each row
#'
#' Substitute for `rowSums(is.na(data))`, but it also checks if input is NULL or
#'   is a dataframe
#'
#' @param data a dataframe
#'
#' @return numeric vector of the number of missing values in each row
#'
#' @export
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]() [n_complete]() [n_complete_row]() [n_miss]() [n_miss_row]() [pct_complete]() [pct_miss]() [prop_complete]() [prop_complete_row]() [prop_miss]()
#'
#' @examples
#'
#' n_miss_row(airquality)
#' n_miss_row(pedestrian)
#'

n_miss_row <- function(data){
  test_if_null(data)
  test_if_dataframe(data)
  as.integer(rowSums(is.na(data)))
}

#' Return a vector of the number of complete values in each row
#'
#' Substitute for `rowSums(!is.na(data))` but it also checks if input is NULL or
#'   is a dataframe
#'
#' @param data a dataframe
#'
#' @return numeric vector of the number of complete values in each row
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]() [n_complete]() [n_complete_row]() [n_miss]() [n_miss_row]() [pct_complete]() [pct_miss]() [prop_complete]() [prop_complete_row]() [prop_miss]()
#'
#' @export
#'
#' @examples
#'
#' n_complete_row(airquality)
#' n_complete_row(pedestrian)
#'

n_complete_row <- function(data){
  test_if_null(data)
  test_if_dataframe(data)
  as.integer(rowSums(!is.na(data)))
}

#' Return a vector of the proportion of missing values in each row
#'
#' Substitute for `rowMeans(is.na(data))`, but it also checks if input is NULL or
#'   is a dataframe
#'
#' @param data a dataframe
#'
#' @return numeric vector of the proportion of missing values in each row
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]() [n_complete]() [n_complete_row]() [n_miss]() [n_miss_row]() [pct_complete]() [pct_miss]() [prop_complete]() [prop_complete_row]() [prop_miss]()
#'
#' @export
#'
#' @examples
#'
#' prop_miss_row(airquality)
#' prop_miss_row(pedestrian)
#'
prop_miss_row <- function(data){
  test_if_null(data)
  test_if_dataframe(data)
  rowMeans(is.na(data))
}

#' Return a vector of the proportion of missing values in each row
#'
#' Substitute for `rowMeans(!is.na(data))`, but it also checks if input is NULL or
#'   is a dataframe
#'
#' @param data a dataframe
#'
#' @return numeric vector of the proportion of missing values in each row
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]() [n_complete]() [n_complete_row]() [n_miss]() [n_miss_row]() [pct_complete]() [pct_miss]() [prop_complete]() [prop_complete_row]() [prop_miss]()
#'
#' @export
#'
#' @examples
#'
#' prop_complete_row(airquality)
#' prop_complete_row(pedestrian)
#'
prop_complete_row <- function(data){
  test_if_null(data)
  test_if_dataframe(data)
  rowMeans(!is.na(data))
}
