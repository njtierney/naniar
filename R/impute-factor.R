#' Impute a factor value into a vector with missing values
#'
#' For imputing fixed factor levels. It adds the new imputed value to the end
#'   of the levels of the vector. We generally recommend to impute using other
#'   model based approaches. See the `simputation` package, for example
#'   [simputation::impute_lm()].
#'
#' @param x vector
#' @param value factor to impute
#'
#' @return vector with a factor values replaced
#' @export
#' @name impute_factor
#'
#' @examples
#'
#' vec <- factor(LETTERS[1:10])
#'
#' vec[sample(1:10, 3)] <- NA
#'
#' vec
#'
#' impute_factor(vec, "wat")
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
impute_factor <- function(x, value) UseMethod("impute_factor")

#' @export
#' @rdname impute_factor
impute_factor.default <- function(x, value) {
  vctrs::vec_assert(x, ptype = character())
}

#' @export
#' @rdname impute_factor
impute_factor.factor <- function(x, value) {
  x <- forcats::fct_expand(x, value)

  x[is.na(x)] <- factor(value)

  x
}

#' @export
#' @rdname impute_factor
impute_factor.character <- function(x, value) {
  x <- forcats::fct_expand(x, value)

  x[is.na(x)] <- factor(value)

  x
}

#' @export
#' @rdname impute_factor
impute_factor.shade <- function(x, value) {
  #do nothing
  x
}
