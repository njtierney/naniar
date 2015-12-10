#' shadow_shift
#'
#' \code{shadow_shift} transforms missing values of a given variable
#'
#' @param x is a variable, must be continuous
#'
#' @import dplyr
#'
#' @export

# Make a window function that transforms missing values to be 10% below the minimum value for that variable
shadow_shift <- function(x){
  ifelse(is.na(x),
         yes = min(x, na.rm = T)*0.9,
         no = x)
  # min() might change to something related to the data range
  # possibly use range() to determine the shadow shift
  # Need to also add some jitter/noise to these points to seperate out repeats of the same value
  # for factors, need to add another level (smaller than smallest)
  # need to think about how time is handled as well.
}
