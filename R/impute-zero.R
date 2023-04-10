#' Impute zero into a vector with missing values
#'
#' This can be useful if you are imputing specific values, however we would
#'   generally recommend to impute using other model based approaches. See
#'   the `simputation` package, for example [simputation::impute_lm()].
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
#' vec
#'
#' impute_zero(vec)
#'
#' library(dplyr)
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
#' @rdname impute_zero
impute_zero <- function(x){

  impute_fixed(x = x, value = 0)

}
