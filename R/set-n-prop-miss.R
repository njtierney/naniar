#' Set a proportion or number of missing values
#'
#' @param x vector of values to set missing
#' @param prop proportion of values between 0 and 1 to set as missing
#'
#' @return vector with missing values added
#' @name set-prop-n-miss
#'
#' @examples
#' vec <- rnorm(5)
#' set_prop_miss(vec, 0.2)
#' set_prop_miss(vec, 0.4)
#' set_n_miss(vec, 1)
#' set_n_miss(vec, 4)
#' @export
set_prop_miss <- function(x, prop = 0.1) {
  check_is_scalar(prop)
  check_btn_0_1(prop)
  x[sample(seq_along(x) <= prop * length(x))] <- NA
  x
}

#' @rdname set-prop-n-miss
#' @param n number of values to set missing
#' @export
set_n_miss <- function(x, n = 1) {
  check_is_scalar(n)
  check_is_integer(n)
  x[sample(seq_along(x) <= n)] <- NA
  x
}
