#' Label a missing from one column
#'
#' Label whether a value is missing in a row of one columns.
#'
#' @param x1 a variable of a dataframe
#'
#' @return a vector indicating whether any of these rows had missing values
#'
#' @note can we generalise label_miss to work for any number of variables?
#'
#' @export
#'
#' @seealso [add_any_miss()] [add_label_missings()] [add_label_shadow()]
#'
#' @examples
#'
#' label_miss_1d(airquality$Ozone)
#'
label_miss_1d <- function(x1) {
  # Catch NULL entries
  test_if_null(x1)
  # find which are missing and which are not.
  temp <- data.frame(x1) %>% is.na %>% rowSums()
  # Were all values in x1 originally NA, they would be cast to factor of 1
  # and assigned mapped_discrete class. Lets cast it back to NA
  if (all(x1 == 1) && inherits(x1, "mapped_discrete")) {
    x1[] <- NA
  }

  as_missing_factor(temp)
}

as_missing_factor <- function(x) {
  # factor assures that Missing and Not Missing will always have same colour
  fct <- factor(
    x = ifelse(
      x == 0, # 0 means not missing, 1 means missing
      "Not Missing", # not missing
      "Missing"
    ), # missing
    levels = c("Not Missing", "Missing")
  )
  stats::relevel(fct, "Missing")
}

#' label_miss_2d
#'
#' Label whether a value is missing in either row of two columns.
#'
#' @param x1 a variable of a dataframe
#' @param x2 another variable of a dataframe
#'
#' @return a vector indicating whether any of these rows had missing values
#' @export
#'
#' @examples
#'
#' label_miss_2d(airquality$Ozone, airquality$Solar.R)
#'
label_miss_2d <- function(x1, x2) {
  # Catch NULL entries
  if (is.null(x1) | is.null(x2)) {
    cli::cli_abort(
      c(
        "Input cannot be NULL",
        "We see the first argument, {.arg x1} is: {.cls {class(x1)}}",
        "We see the second argument, {.arg x2} is: {.cls {class(x2)}}"
      )
    )
  }
  # find which are missing and which are not.
  # Were all values in x1/x2 originally NA, they would be cast to factor of 1
  # and assigned mapped_discrete class. Lets cast it back to NA
  if (all(x1 == 1) && inherits(x1, "mapped_discrete")) {
    x1[] <- NA
  }

  if (all(x2 == 1) && inherits(x2, "mapped_discrete")) {
    x2[] <- NA
  }

  temp <- data.frame(x1, x2) %>% is.na %>% rowSums()

  as_missing_factor(temp)
}
