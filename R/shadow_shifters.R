#' Shift missing values to 10\% below minimum value
#'
#' shadow_shift is a window function that transforms missing values to be about 10% below the minimum value for a given variable, plus some jittered noise, to separate repeated values, so that missing values can be visualised along with the rest of the data
#'
#' @param x is a variable, must be continuous
#'
#' @examples
#' airquality$Ozone
#' shadow_shift(airquality$Ozone)
#' library(dplyr)
#' airquality %>%
#'     mutate(Ozone_shift = shadow_shift(Ozone))
#'
#' @export
# Constructor function ---------------------------------------------------------
# create the S3 method
shadow_shift <- function(x) UseMethod("shadow_shift")

# NULL -------------------------------------------------------------------------

#' @export
shadow_shift.NULL <- function(x) NULL

# default ----------------------------------------------------------------------

#' @export
shadow_shift.default <- function(x){
  stop(
    "shadow_shift does not know how to deal with data of class ",
    class(x),
    "please check your input is more than length one",
    call. = FALSE
  )

}

#' @export
shadow_shift.numeric <- function(x){


  # add an exception for when length x == 1
  if(n_complete(x) == 1){

    xmin <- min(x, na.rm = T)

    x_shift <- xmin - xmin*0.1

    x_jitter <- (stats::runif(length(x))-0.50)*x_shift*0.10

    ifelse(is.na(x),
           yes = x_shift + x_jitter,
           no = x)

    # else, when there is more than 1 complete value
  } else {

  xrange <- max(x, na.rm = T) - min(x, na.rm = T)

  xmin <- min(x, na.rm = T)

  # create the "jitter" to be added around the points.
  xrunif <- (stats::runif(length(x))-0.5)*xrange*0.05

  ifelse(is.na(x),
         # add the jitter around the those values that are missing
         yes = xmin-xrange*0.1 + xrunif,
         no = x)

  } # close else statement

} # close function
