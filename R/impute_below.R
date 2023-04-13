#' Impute data with values shifted 10 percent below range.
#'
#' It can be useful in exploratory graphics to impute data outside the range of
#'   the data. `impute_below` imputes variables with missings to have values
#'   10 percent below the range for numeric values, plus some jittered noise,
#'   to separate repeated values, so that missing values can be visualised
#'   along with the rest of the data. For character or factor
#'   values, it adds a new string or label.
#'
#' @param x a variable of interest to shift
#' @param ... extra arguments to pass
#'
#' @seealso [add_shadow_shift()] [cast_shadow_shift()] [cast_shadow_shift_label()]
#'
#' @export
#' @examples
#'library(dplyr)
#'vec <- rnorm(10)
#'
#'vec[sample(1:10, 3)] <- NA
#'
#'impute_below(vec)
#'impute_below(vec, prop_below = 0.25)
#'impute_below(vec,
#'             prop_below = 0.25,
#'             jitter = 0.2)
#'
#'dat <- tibble(
#'  num = rnorm(10),
#'  int = as.integer(rpois(10, 5)),
#'  fct = factor(LETTERS[1:10])
#') %>%
#'  mutate(
#'    across(
#'      everything(),
#'      \(x) set_prop_miss(x, prop = 0.25)
#'    )
#'  )
#'
#'dat
#'
#'dat %>%
#'  nabular() %>%
#'  mutate(
#'    num = impute_below(num),
#'    int = impute_below(int),
#'    fct = impute_below(fct),
#'  )
#'
#'dat %>%
#'  nabular() %>%
#'  mutate(
#'    across(
#'      where(is.numeric),
#'      impute_below
#'    )
#'  )
#'
#'dat %>%
#'  nabular() %>%
#'  mutate(
#'    across(
#'      c("num", "int"),
#'      impute_below
#'    )
#'  )
#'
#'
impute_below <- function(x, ...) UseMethod("impute_below")

#' @export
impute_below.NULL <- function(x, ...) NULL

#' @export
impute_below.default <- function(x, ...){
  cli::cli_abort(
    c(
      "{.fun impute_below} does not know how to deal with data of class {.cls {class_glue(x)}}",
      "Check if your input is more than length one, and that you are using the right function. Perhaps you meant to apply this to many variables in a data frame? See the examples dor details on doing this with {.fun across}"
    )
  )

}

# function to perform the shifting/imputing, which is used by later function
shift_values <- function(x,
                         xmin,
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

#' Impute numeric values below a range for graphical exploration
#'
#' @param x a variable of interest to shift
#' @param prop_below the degree to shift the values. default is
#' @param jitter the amount of jitter to add. default is 0.05
#' @param seed_shift a random seed to set, if you like
#' @param ... extra arguments to pass
#' @export
impute_below.numeric <- function(x,
                                 prop_below = 0.1,
                                 jitter = 0.05,
                                 seed_shift = 2017-7-1-1850,
                                 ...){

  # add an exception for cases with infinite values
  if (any(is.infinite(x))) {

    # use the minimum for the non infinite values
    xmin <- min(x[!is.infinite(x)], na.rm = TRUE)

    shifted_values <- shift_values(x,
                                   xmin,
                                   prop_below,
                                   seed_shift,
                                   jitter)

    return(shifted_values)

  }

  # add an exception for when length x == 1 and variance is zero
  if (n_complete(x) == 1 | stats::var(x, na.rm = TRUE) == 0) {

    xmin <- min(x, na.rm = TRUE)

    shifted_values <- shift_values(x,
                                   xmin,
                                   prop_below,
                                   seed_shift,
                                   jitter)

    return(shifted_values)

    # else, when there is more than 1 complete value
  }

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

} # close function

#' @export
impute_below.factor <- function(x, ...){
  forcats::fct_na_value_to_level(x, level = "missing")
}

#' @export
impute_below.character <- function(x, ...){
  dplyr::if_else(is.na(x),
                 true = "missing",
                 false = x)
}

#' Impute data with values shifted 10 percent below range.
#'
#' It can be useful in exploratory graphics to impute data outside the range of
#'   the data. `impute_below_all` imputes all variables with missings to have
#'   values 10\% below the range for numeric values, and for character or factor
#'   values adds a new string or label.
#'
#' `r lifecycle::badge('superseded')`
#'
#' @param .tbl a data.frame
#' @param prop_below the degree to shift the values. default is
#' @param jitter the amount of jitter to add. default is 0.05
#' @param ... additional arguments
#'
#' @return an dataset with values imputed
#' @export
#'
#' @examples
#'
#' # you can impute data like so:
#' airquality %>%
#'   impute_below_all()
#'
#' # However, this does not show you WHERE the missing values are.
#' # to keep track of them, you want to use `bind_shadow()` first.
#'
#' airquality %>%
#'   bind_shadow() %>%
#'   impute_below_all()
#'
#' # This identifies where the missing values are located, which means you
#' # can do things like this:
#'
#' \dontrun{
#' library(ggplot2)
#' airquality %>%
#'   bind_shadow() %>%
#'   impute_below_all() %>%
#'   # identify where there are missings across rows.
#'   add_label_shadow() %>%
#'   ggplot(aes(x = Ozone,
#'              y = Solar.R,
#'              colour = any_missing)) +
#'   geom_point()
#' # Note that this ^^ is a long version of `geom_miss_point()`.
#' }
#'
impute_below_all <- function(.tbl,
                             prop_below = 0.1,
                             jitter = 0.05,
                             ...){

  lifecycle::signal_stage("superseded", "impute_below_all()")

  test_if_dataframe(.tbl)
  test_if_null(.tbl)

  dplyr::mutate_all(.tbl = .tbl,
                    .funs = impute_below,
                    prop_below = prop_below,
                    jitter = jitter)

}

#' Scoped variants of `impute_below`
#'
#'  `impute_below` imputes missing values to a set percentage below the range
#'   of the data. To impute many variables at once, we recommend that you use the
#'  `across` function workflow, shown in the examples for [impute_below()].
#'  `impute_below_all` operates on all variables. To only impute variables
#'   that satisfy a specific condition, use the scoped variants,
#'   `impute_below_at`, and `impute_below_if`. To use `_at` effectively,
#'   you must know that `_at`` affects variables selected with a character
#'   vector, or with `vars()`.
#'
#' `r lifecycle::badge('superseded')`
#'
#' @param .tbl a data.frame
#' @param .vars variables to impute
#' @param prop_below the degree to shift the values. default is
#' @param jitter the amount of jitter to add. default is 0.05
#' @param ... extra arguments
#'
#' @return an dataset with values imputed
#' @export
#'
#' @examples
#' # select variables starting with a particular string.
#' impute_below_at(airquality,
#'                 .vars = c("Ozone", "Solar.R"))
#'
#' impute_below_at(airquality, .vars = 1:2)
#'
#' \dontrun{
#' library(dplyr)
#' impute_below_at(airquality,
#'                 .vars = vars(Ozone))
#'
#' library(ggplot2)
#' airquality %>%
#'   bind_shadow() %>%
#'   impute_below_at(vars(Ozone, Solar.R)) %>%
#'   add_label_shadow() %>%
#'   ggplot(aes(x = Ozone,
#'              y = Solar.R,
#'              colour = any_missing)) +
#'          geom_point()
#' }
#'
impute_below_at <- function(.tbl,
                            .vars,
                            prop_below = 0.1,
                            jitter = 0.05,
                            ...){

  lifecycle::signal_stage("superseded", "impute_below_at()")

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_at(.tbl = .tbl,
                   .vars = .vars,
                   .funs = impute_below,
                   prop_below = prop_below,
                   jitter = jitter)
}

#' Scoped variants of `impute_below`
#'
#' `impute_below` operates on all variables. To only impute variables
#'   that satisfy a specific condition, use the scoped variants,
#'   `impute_below_at`, and `impute_below_if`.
#'
#' @param .tbl data.frame
#' @param .predicate A predicate function (such as is.numeric)
#' @param prop_below the degree to shift the values. default is
#' @param jitter the amount of jitter to add. default is 0.05
#' @param ... extra arguments
#'
#' @return an dataset with values imputed
#' @export
#' @examples
#'
#' airquality %>%
#'   impute_below_if(.predicate = is.numeric)
#'
impute_below_if <- function(.tbl,
                            .predicate,
                            prop_below = 0.1,
                            jitter = 0.05,
                            ...){

  lifecycle::signal_stage("superseded", "impute_below_if()")

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_if(.tbl = .tbl,
                   .predicate = .predicate,
                   .funs = impute_below,
                   prop_below = prop_below,
                   jitter = jitter)
}
