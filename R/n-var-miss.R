#' The number of variables or cases with missing values
#'
#' This function calculates the number of variables or cases that contain a missing value
#'
#' @param data data.frame
#'
#' @return integer, number of missings
#' @seealso [n_var_complete()]
#' @name n-var-case-miss
#' @export
#'
#' @examples
#'
#' # how many variables contain missing values?
#' n_var_miss(airquality)
#' n_case_miss(airquality)
#'
n_var_miss <- function(data){
  sum(colSums(is.na(data)) != 0)
}

#' @export
#' @rdname n-var-case-miss
n_case_miss <- function(data){
  sum(rowSums(is.na(data)) != 0)
}

#' The number of variables with complete values
#'
#' This function calculates the number of variables that contain a complete value
#'
#' @param data data.frame
#'
#' @return integer number of complete values
#' @seealso [n_var_miss()]
#' @name n-var-case-complete
#' @export
#'
#' @examples
#'
#' # how many variables contain complete values?
#' n_var_complete(airquality)
#' n_case_complete(airquality)
#'
n_var_complete <- function(data){
  sum(colSums(is.na(data)) == 0)
}

#' @export
#' @name n-var-case-complete
n_case_complete <- function(data){
  sum(rowSums(is.na(data)) == 0)
}
