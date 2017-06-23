#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

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
#' narnia:::where_na(airquality)
#'
where_na <- function(x){
  which(is.na(x), arr.ind = TRUE)
}

#' Which elements contain missings?
#'
#' Internal function that creates a matrix containing the location of missing
#'   values in a dataframe. This may be used in the future `impl_df` class.
#'
#' @param x a dataframe
#'
#' @return a matrix with columns "row" and "col", which refer to the row and
#'     column that identify the position of a missing value in a dataframe
#'
#' @seealso where_na
#'
#' @examples
#'
#' narnia:::which_na(airquality)
#'
which_na <- function(x){
  which(is.na(x))
}

# note:
# it would be cool for the missing data mechanisms if these were also treated as regular missing values in the other parts of narnia.
# on that thought, they should just be regular missing values, but the addition of something like the `as_shadow` argument allows for them to be missing with specified structure.
# so, I guess what I'm saying is that there should be a way for the proposed missing mechanisms to be treated differently, if desired, otherwise, treated as regular missings, so they are treated the same by the rest of the R universe.


#' Helper function to determine whether there are any missings
#'
#' @param x a vector
#'
#' @return logical vector TRUE = missing FALSE = complete
#'
any_row_miss <- function(x){
  apply(data.frame(x), MARGIN = 1, FUN = function(x) anyNA(x))
}

#' Helper function to determine whether all rows are missing
#'
#' @param x a vector
#'
#' @return logical vector
all_row_miss <- function(x){
  apply(data.frame(x), MARGIN = 1, FUN = function(x) all(is.na(x)))
}

#' Helper function to determine whether all rows are complete
#'
#' @param x a vector
#'
#' @return logical vector
all_row_complete <- function(x){
  apply(data.frame(x), MARGIN = 1, FUN = function(x) all(!is.na(x)))
}
