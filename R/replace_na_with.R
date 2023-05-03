#' Replace NA value with provided value
#'
#' This function helps you replace NA values with a single provided value.
#'   This can be classed as a kind of imputation, and is powered by
#'   [impute_fixed()]. See [tidyr::replace_na()] for a slightly different
#'   approach, [dplyr::coalesce()] for replacing NAs with values from other
#'   vectors, and [dplyr::na_if()] to replace specified values with NA.
#'
#' @return vector with replaced values
#' @export
#'
#' @examples
#'
#' library(naniar)
#' x <- c(1:5, NA, NA, NA)
#' x
#' replace_na_with(x, 0L)
#'
#' library(dplyr)
#' dat <- tibble(
#'   ones = c(NA,1,1),
#'   twos = c(NA,NA, 2),
#'   threes = c(NA, NA, NA)
#' )
#'
#' dat
#'
#' dat %>%
#'   mutate(
#'     ones = replace_na_with(ones, 0),
#'     twos = replace_na_with(twos, -2),
#'     threes = replace_na_with(threes, -3)
#'   )
#'
#' dat %>%
#'   mutate(
#'     across(
#'       everything(),
#'       \(x) replace_na_with(x, -99)
#'     )
#'   )
#'
replace_na_with <- function(x, value) {
  impute_fixed(x, value)
}
