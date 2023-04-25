#' Shift missing values to facilitate missing data exploration/visualisation
#'
#' `shadow_shift` transforms missing values to facilitate visualisation, and has
#'   different behaviour for different types of variables. For numeric
#'   variables, the values are shifted to 10% below the minimum value for a given
#'   variable plus some jittered noise, to separate repeated values, so that
#'   missing values can be visualised along with the rest of the data.
#'
#' `r lifecycle::badge('deprecated')`
#'
#' @param ... arguments to [impute_below()].
#'
#' @seealso [add_shadow_shift()] [cast_shadow_shift()] [cast_shadow_shift_label()]
#'
#' @examples
#' airquality$Ozone
#' shadow_shift(airquality$Ozone)
#' \dontrun{
#' library(dplyr)
#' airquality %>%
#'     mutate(Ozone_shift = shadow_shift(Ozone))
#' }
#' @export
shadow_shift <- function(...){
  lifecycle::deprecate_soft("1.1.0", "shadow_shift()", "impute_below()")
  impute_below(...)
}

