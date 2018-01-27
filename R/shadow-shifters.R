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

#' @export
shadow_shift.numeric <- function(x, seed_shift = 2017-7-1-1850, ...){

  # add an exception for cases with infinite values
  if (any(is.infinite(x))) {

    # use the minimum for the non infinite values
    xmin <- min(df_inf$x[!is.infinite(df_inf$x)], na.rm = TRUE)

    x_shift <- xmin - xmin*0.1

    # set the seed here
    set.seed(seed_shift)
    x_jitter <- (stats::runif(length(x))-0.50)*x_shift*0.10

    # overwrite x
    x <- ifelse(is.na(x),
           yes = x_shift + x_jitter,
           no = x)

    # exit early, no need to move through the rest
    return(x)

  }

  # add an exception for when length x == 1 and variance is zero
  if (n_complete(x) == 1 | stats::var(x, na.rm = TRUE) == 0) {

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
