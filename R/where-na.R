#' Which rows and cols contain missings?
#'
#' Internal function that is short for `which(is.na(x), arr.ind = TRUE)`.
#'   Creates array index locations of missing values in a dataframe.
#'
#' @param x a dataframe
#'
#' @return a matrix with columns "row" and "col", which refer to the row and
#'     column that identify the position of a missing value in a dataframe
#'
#' @seealso [which_na()]
#'
#' @examples
#'
#' where_na(airquality)
#' where_na(oceanbuoys$sea_temp_c)
#'
#' @export
#'
where_na <- function(x) {
  which(is.na(x), arr.ind = TRUE)
}

#' Which elements contain missings?
#'
#' Equivalent to `which(is.na())` - returns integer locations of missing values.
#'
#' @param x a dataframe
#'
#' @return integer locations of missing values.
#'
#' @seealso [where_na()]
#'
#' @examples
#'
#' which_na(airquality)
#'
#' @export
#'
which_na <- function(x) {
  which(is.na(x))
}


#' Which variables contain missing values?
#'
#' It can be helpful when writing other functions to just return the names
#'   of the variables that contain missing values. `miss_var_which` returns a
#'   vector of variable names that contain missings. It will return NULL when
#'   there are no missings.
#'
#' @param data a data.frame
#'
#' @return character vector of variable names
#'
#' @export
#'
#' @examples

#' miss_var_which(airquality)
#'
#' miss_var_which(mtcars)

#'
miss_var_which <- function(data) {
  # basic type tests
  test_if_dataframe(data)

  test_if_missing(data)

  test_if_null(data)

  # if there are no missing values, return NULL
  if (!anyNA(data)) {
    return(NULL)
  }

  # else return variables that contain ANY missing values
  na_vars <- names(which(colSums(is.na(data)) > 0))

  return(na_vars)
}
