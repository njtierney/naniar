#' Which rows and cols contain missings?
#'
#' Internal function that creates a matrix containing the location of missing
#' values in a dataframe. This may be used in the future `impl_df` class.
#'
#' @param x a dataframe
#'
#' @return a matrix with columns "row" and "col", which refer to the row and
#'     column that identify the position of a missing value in a dataframe
#'
#' @examples
#'
#' narnia:::which_na(airquality)
#'
which_na <- function(x){
  which(is.na(x), arr.ind = TRUE)
}
