#' Return a vector of the number of missing values in each row
#'
#' Substitute for `rowSums(is.na(data))`
#'
#' @param data a dataframe
#'
#' @return numeric vector of the number of missing values in each row
#'
#' @export
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
#' Substitute for `rowSums(!is.na(data))`
#'
#' @param data a dataframe
#'
#' @return numeric vector of the number of complete values in each row
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
#' Substitute for `rowMeans(is.na(data))`
#'
#' @param data a dataframe
#'
#' @return numeric vector of the proportion of missing values in each row
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
#' Substitute for `rowMeans(is.na(data))`
#'
#' @param data a dataframe
#'
#' @return numeric vector of the proportion of missing values in each row
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
