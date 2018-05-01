#' Shift missing values to facilitate missing data exploration/visualisation
#'
#' `shadow_shift` transforms missing values to facilitate visualisation, and has
#'   different behaviour for different types of variables. For numeric
#'   variables, the values are shifted to 10% below the minimum value for a given
#'   variable plus some jittered noise, to separate repeated values, so that
#'   missing values can be visualised along with the rest of the data.
#'
#' @param x a variable of interest to shift
#' @param ... extra arguments to pass
#'
#' @seealso [add_shadow_shift()] [cast_shadow_shift()] [cast_shadow_shift_label()]
#'
#' @examples
#' airquality$Ozone
#' shadow_shift(airquality$Ozone)
#' library(dplyr)
#' airquality %>%
#'     mutate(Ozone_shift = shadow_shift(Ozone))
#'
#' @export
shadow_shift <- function(x, ...) UseMethod("shadow_shift")

#' @export
shadow_shift.NULL <- function(x, ...) NULL

#' @export
shadow_shift.default <- function(x, ...){
  stop(
    "shadow_shift does not know how to deal with data of class ",
    class(x),
    " please check your input is more than length one",
    call. = FALSE
  )

}

#' Shift (impute) numeric values for graphical exploration
#'
#' @param x a variable of interest to shift
#' @param prop_below the degree to shift the values. default is
#' @param jitter the amount of jitter to add. deafult is 0.05
#' @param seed_shift a random seed to set, if you like
#' @param ... extra arguments to pass
#' @export
shadow_shift.numeric <- function(x,
                                 prop_below = 0.1,
                                 jitter = 0.05,
                                 seed_shift = 2017-7-1-1850,
                                 ...){

  # function to perform the shifting/imputing, which is used by later function
  shift_values <- function(xmin,
                           prop_below,
                           seed_shift,
                           jitter) {

    # provide the amount of shift - default is 0.1
    x_shift <- xmin - xmin * prop_below

    # set the seed here
    set.seed(seed_shift)
    x_jitter <- (stats::runif(length(x)) - 0.50) * x_shift * jitter

    # overwrite x
    x <- ifelse(is.na(x),
                yes = x_shift + x_jitter,
                no = x)

    return(x)

  }

  # add an exception for cases with infinite values
  if (any(is.infinite(x))) {

    # use the minimum for the non infinite values
    xmin <- min(x[!is.infinite(x)], na.rm = TRUE)

    shifted_values <- shift_values(xmin,
                                  prop_below,
                                  seed_shift,
                                  jitter)

    return(shifted_values)

  }

  # add an exception for when length x == 1 and variance is zero
  if (n_complete(x) == 1 | stats::var(x, na.rm = TRUE) == 0) {

    xmin <- min(x, na.rm = TRUE)

    shifted_values <- shift_values(xmin,
                                   prop_below,
                                   seed_shift,
                                   jitter)

    return(shifted_values)

    # else, when there is more than 1 complete value
  } else {

  range_dist <- function(x) diff(range(x, na.rm = TRUE))

  xrange <- range_dist(x)

  xmin <- min(x, na.rm = TRUE)

  # create the "jitter" to be added around the points.
  set.seed(seed_shift)
  x_jitter <- (stats::runif(length(x)) - 0.5) * xrange * jitter

  x_shift <- xmin - xrange * prop_below

  ifelse(is.na(x),
         # add the jitter around the those values that are missing
         yes = x_shift + x_jitter,
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
