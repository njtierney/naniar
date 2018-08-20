#' Test if input is or are shadow variables
#'
#' Shadow matrix or "nabular" data is a useful way to store missing data to
#'   facilitate missing data visualisation. This data can be created using
#'   `bind_shadow`. `is_shadow` tells us if there are any shadow variables.
#'
#' @param x a vector or data.frame
#'
#' @return logical vector of length 1
#'
#' @examples
#'
#' aq_sh <- as_shadow(airquality)
#' aq_bind <- bind_shadow(airquality)
#'
#' is_shadow(aq_sh)
#' is_shadow(airquality)
#' is_shadow(aq_bind)
#'
#' @export
#' @name is_shadow

is_shadow <- function(x){
  inherits(x, "shadow")
}
