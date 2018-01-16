#' Replace all values with NA where a certain condition is met
#'
#' This function takes a dataframe and replaces all values that meet the
#'   condition specified as an NA value, following a special syntax.
#'
#' @param data A dataframe
#' @param .funs A condition required to be TRUE to set NA. Here, the condition
#'   is specified with a formula, following the syntax: `~.x {condition}`.
#'   For example, writing `~.x < 20` would mean "where a variable value is less
#'   than 20, replace with NA".
#'
#' @examples

#' dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#'                           1,   "A",   -100,
#'                           3,   "N/A", -99,
#'                           NA,  NA,    -98,
#'                           -99, "E",   -101,
#'                           -98, "F",   -1)
#'
#' dat_ms

#' #replace all instances of -99 with NA
#' replace_with_na_all(data = dat_ms,
#'                     .funs = ~.x == -99)
#'
#' # replace all instances of -98 with NA
#' replace_with_na_all(data = dat_ms,
#'                     .funs = ~.x == -98)
#'
#' # replace all instances of -99 or -98 with NA
#' replace_with_na_all(dat_ms,
#'                     .funs = ~.x %in% c(-99, -98))
#'
#' # replace all instances of -99 or -98, or "N/A" with NA
#' replace_with_na_all(dat_ms,
#'                     .funs = ~.x %in% c(-99, -98, "N/A"))
#'
#' # where works with functions
#' replace_with_na_all(airquality, ~ sqrt(.x) < 5)
#'
#' @export
replace_with_na_all <- function(data, .funs) {
  purrr::map_df(data, ~ na_set(.x, .funs) )
}

# Future work
  # replace_with_na_all(airquality, . < 20)
  # replace_with_na_all(airquality, x < 20)
  # replace_with_na_all(airquality, function(x) mean(x) < 20)

#' Replace specified variables with NA where a certain condition is met
#'
#' @param data dataframe
#' @param .vars The variables to refer to
#' @param .funs A condition required to be TRUE to set NA. Here, the condition
#'   is specified with a formula, following the syntax: `~.x {condition}`.
#'   For example, writing `~.x < 20` would mean "where a variable value is less
#'   than 20, replace with NA".
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#'
#' dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#'                           1,   "A",   -100,
#'                           3,   "N/A", -99,
#'                           NA,  NA,    -98,
#'                           -99, "E",   -101,
#'                           -98, "F",   -1)
#'
#' dat_ms
#'
#' replace_with_na_at(data = dat_ms,
#'                  .vars = "x",
#'                  .funs = ~.x == -99)
#'
#' replace_with_na_at(data = dat_ms,
#'                  .vars = c("x","z"),
#'                  .funs = ~.x == -99)
#'

#'
replace_with_na_at <- function(data, .vars, .funs) {
  test_if_dataframe(data)
  test_if_null(data)
  test_if_missing(data)
  purrr::modify_at(data, .vars, ~ na_set(.x, .funs))
}

# Future work
   # use of `vars` - `vars(var1, var2)`
   # replace_with_na_at(tbl = airquality, .at = vars(Wind, Ozone), ~ .x < 20)

#' Replace values with NA based on some condition, for variables that meet some predicate
#'
#' @param data Dataframe
#' @param .predicate A predicate function to be applied to the columns or a
#'   logical vector.
#' @param .funs A condition required to be TRUE to set NA. Here, the condition
#'   is specified with a formula, following the syntax: `~.x {condition}`.
#'   For example, writing `~.x < 20` would mean "where a variable value is less
#'   than 20, replace with NA".
#'
#' @return Dataframe
#' @export
#'
#' @examples
#'
#' dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#'                           1,   "A",   -100,
#'                           3,   "N/A", -99,
#'                           NA,  NA,    -98,
#'                           -99, "E",   -101,
#'                           -98, "F",   -1)
#'
#' dat_ms
#'
#' replace_with_na_if(data = dat_ms,
#'                  .predicate = is.character,
#'                  .funs = ~.x == "N/A")
#'
#' replace_with_na(dat_ms,
#'               to_na = list(x = c(-99, -98),
#'                            y = c("N/A"),
#'                            z = c(-101)))
#'
#'
replace_with_na_if <- function(data, .predicate, .funs) {
  test_if_dataframe(data)
  test_if_null(data)
  test_if_missing(data)
  purrr::modify_if(data, .predicate, ~ na_set(.x, .funs))
}

#' Replace values in dataframe columns with NA where some condition is met
#'
#' This code is currently in development, and is taken directly from TJ Mahr's
#'   package fillgaze:
#'   https://github.com/tjmahr/fillgaze/blob/master/R/fillgaze-package.R#L11
#'   for the purposes of testing and experimentation.
#'
#' @param data a dataframe
#' @param ... predicate functions that return true whenever a value should be
#'   replaced with NAs. The functions should be named, so that the argument
#'   `var1 = is.finite` would replace all the values in the column `var1` where
#'   `is.finite()` returns `TRUE` with `NA`` values. These predicate functions
#'   can be defined using the [formula syntax for anonymous
#'   functions][rlang::as_function].
#' @return a modified copy of the dataframe
#' @export
#' @examples
#' is_zero <- function(x) x == 0
#' replace_with_na_where(mtcars, cyl = cyl == 6, vs = is_zero)
replace_with_na_where <- function(data, ...) {
  dots <- rlang::quos(...)
  stopifnot(names(dots) %in% names(data), !anyDuplicated(names(dots)))

  set_to_na <- lapply(dots, rlang::eval_tidy, data = data)

  for (col in names(set_to_na)) {
    data[set_to_na[[col]], col] <- NA
  }

  data
}

# utility funs for replace_with_na_*  ------------------------------------------

create_mapper_na <- function(.funs){
  glue::glue("~ {rlang::f_text(.funs)} & !is.na(.x)") %>%
    as.formula() %>%
    purrr::as_mapper()
}

na_set <- function(vec, .funs) {
  # modify this vector with this function, return NA
  purrr::modify_if(vec, create_mapper_na(.funs) , ~ NA) %>%
    # flatten this out into a regular vector
    purrr::reduce(c)

  # na_set(aq_small$Ozone, ~ .x < 20)
}
