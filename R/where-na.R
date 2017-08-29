#' Which rows and cols contain missings?
#'
#' Internal function that is short for `which(is.na(x))`. Creates integer
#'   locations of missing values in a dataframe. May be used in future `impl_df`
#'   class.
#'
#' @param x a dataframe
#'
#' @return integers that describe the location of missing values
#'
#' @seealso which_na
#'
#' @examples
#'
#' where_na(airquality)
#' where_na(oceanbuoys$sea_temp_c)
#'
#' @export
#'
where_na <- function(x){
  which(is.na(x), arr.ind = TRUE)
}
