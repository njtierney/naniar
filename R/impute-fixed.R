#' Impute a fixed value into a vector with missing values
#'
#' This can be useful if you are imputing specific values, however we would
#'   generally recommend to impute using other model based approaches. See
#'   the `simputation` package, for example [simputation::impute_lm()].
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
#' vec
#'
#' impute_fixed(vec, -999)
#'
#' library(dplyr)
#'
#' dat <- tibble(
#'   num = rnorm(10),
#'   int = rpois(10, 5),
#'   fct = factor(LETTERS[1:10])
#' ) %>%
#'   mutate(
#'     across(
#'       everything(),
#'       \(x) set_prop_miss(x, prop = 0.25)
#'     )
#'   )
#'
#' dat
#'
#' dat %>%
#'   nabular() %>%
#'   mutate(
#'     num = impute_fixed(num, -9999),
#'     int = impute_zero(int),
#'     fct = impute_factor(fct, "out")
#'   )
#'
impute_fixed <- function(x, value) UseMethod("impute_fixed")

#' @export
#' @rdname impute_fixed
impute_fixed.default <- function(x, value) {
  x[is.na(x)] <- value

  x
}
