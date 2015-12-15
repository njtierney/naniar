#' shadow_shift
#'
#' \code{shadow_shift} transforms missing values of a given variable
#'
#' @param x is a variable, must be continuous
#'
#' @import dplyr
#'
#' @export

# shadow_shift is a window function that transforms missing values to be about 10% below the minimum value for a given variable, plus some jittered noise, to separate repeated values, so that missing values can be visualised along with the rest of the data

shadow_shift <- function(x){

  xrange <- max(x, na.rm = T) - min(x, na.rm = T)

  xmin <- min(x, na.rm = T)

  # create the "jitter" to be added around the points.
  xrunif <- (runif(length(x))-0.5)*xrange*0.05

  ifelse(is.na(x),
         # add the jitter around the those values that are missing
         yes = xmin-xrange*0.1 + xrunif,
         no = x)

} # close function
