#' Impute a fixed value into a vector with missing values
#'
#' This can be useful if you are imputing specific values, however we would
#'   generally recommend to impute using other model based approaches. See
#'   [impute_lm()] and friends.
#'
#' @param x vector
#' @param value value to impute
#'
#' @return vector with a fixed values replaced
#' @export
#' @name impute_fixed
#'
#' @examples
#'
#' vec <- rnorm(10)
#'
#' vec[sample(1:10, 3)] <- NA
#'
#' impute_fixed(vec, -999)
#'
impute_fixed <- function(x, value) UseMethod("impute_fixed")

#' @export
#' @rdname impute_fixed
impute_fixed.default <- function(x, value){

  x[is.na(x)] <- value

  x
}
