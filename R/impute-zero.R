#' Impute zero into a vector with missing values
#'
#' This can be useful if you are imputing specific values, however we would
#'   generally recommend to impute using other model based approaches. See
#'   [impute_lm()] and friends.
#'
#' @param x vector
#'
#' @return vector with a fixed values replaced
#' @export
#'
#' @examples
#'
#' vec <- rnorm(10)
#'
#' vec[sample(1:10, 3)] <- NA
#'
#' impute_zero(vec)
#' @rdname impute_zero
impute_zero <- function(x){

  impute_fixed(x = x, value = 0)

}
