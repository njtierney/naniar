#' Shift missing values to facilite missing data exploration/visualisation
#'
#' `shadow_shift` is a window function that transforms missing values to
#'     facilitate visualisation. This has different behaviour for different
#'     types of variables. For numeric variables, the values are shifted to 10%
#'     below the minimum value for a given variable plus some jittered noise,
#'     to separate repeated values, so that missing values can be visualised
#'     along with the rest of the data. There are different shifts that can
#'     take place in different kinds of variables, which are currently under
#'     development.
#'
#' @param x a variable of interest to shift
#' @param ... extra arguments to pass
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
shadow_shift <- function(x, ...) UseMethod("shadow_shift")

# NULL -------------------------------------------------------------------------

#' @export
shadow_shift.NULL <- function(x, ...) NULL

# default ----------------------------------------------------------------------

#' @export
shadow_shift.default <- function(x, ...){
  stop(
    "shadow_shift does not know how to deal with data of class ",
    class(x),
    " please check your input is more than length one",
    call. = FALSE
  )

}

#' @export
shadow_shift.numeric <- function(x, seed_shift = 2017-7-1-1850, ...){


  # add an exception for when length x == 1
  if(n_complete(x) == 1 | stats::var(x, na.rm = TRUE) == 0){

    xmin <- min(x, na.rm = TRUE)

    x_shift <- xmin - xmin*0.1

    # set the seed here
    set.seed(seed_shift)
    x_jitter <- (stats::runif(length(x))-0.50)*x_shift*0.10

    ifelse(is.na(x),
           yes = x_shift + x_jitter,
           no = x)

    # else, when there is more than 1 complete value
  } else {

  xrange <- max(x, na.rm = T) - min(x, na.rm = T)

  xmin <- min(x, na.rm = T)

  # create the "jitter" to be added around the points.
  set.seed(seed_shift)
  xrunif <- (stats::runif(length(x))-0.5)*xrange*0.05

  ifelse(is.na(x),
         # add the jitter around the those values that are missing
         yes = xmin-xrange*0.1 + xrunif,
         no = x)

  } # close else statement

} # close function

#' @export
shadow_shift.factor <- function(x, ...){
  forcats::fct_explicit_na(x, na_level = "missing")
}

#' @export
shadow_shift.character <- function(x, ...){
  dplyr::if_else(is.na(x),
                 true = "missing",
                 false = x)
}
#
# library(narnia)
#
# riskfactors %>%
#   add_shadow_shift(vars = c("drink_average", "smoke_stop")) %>%
#   select(drink_average, drink_average_shift, smoke_stop, smoke_stop_shift)
