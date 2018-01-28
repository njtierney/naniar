#' Replace all values with NA where a certain condition is met
#'
#' This function takes a dataframe and replaces all values that meet the
#'   condition specified as an NA value, following a special syntax.
#'
#' @param data A dataframe
#' @param condition A condition required to be TRUE to set NA. Here, the condition
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
#'                     condition = ~.x == -99)
#'
#' # replace all instances of -98 with NA
#' replace_with_na_all(data = dat_ms,
#'                     condition = ~.x == -98)
#'
#' # replace all instances of -99 or -98 with NA
#' replace_with_na_all(dat_ms,
#'                     condition = ~.x %in% c(-99, -98))
#'
#' # replace all instances of -99 or -98, or "N/A" with NA
#' replace_with_na_all(dat_ms,
#'                     condition = ~.x %in% c(-99, -98, "N/A"))
#'
#' # where works with functions
#' replace_with_na_all(airquality, ~ sqrt(.x) < 5)
#'
#' @export
replace_with_na_all <- function(data, condition) {
  purrr::map_df(data, ~ na_set(.x, condition) )
}

# Future work
  # replace_with_na_all(airquality, . < 20)
  # replace_with_na_all(airquality, x < 20)
  # replace_with_na_all(airquality, function(x) mean(x) < 20)

#' Replace specified variables with NA where a certain condition is met
#'
#' @param data dataframe
#' @param .vars The variables to refer to
#' @param condition A condition required to be TRUE to set NA. Here, the condition
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
#'                  condition = ~.x == -99)
#'
#' replace_with_na_at(data = dat_ms,
#'                  .vars = c("x","z"),
#'                  condition = ~.x == -99)
#'
#'
replace_with_na_at <- function(data, .vars, condition) {
  test_if_dataframe(data)
  test_if_null(data)
  test_if_missing(data)
  purrr::modify_at(data, .vars, ~ na_set(.x, condition))
}


#' Replace values with NA based on some condition, for variables that meet some predicate
#'
#' @param data Dataframe
#' @param .predicate A predicate function to be applied to the columns or a
#'   logical vector.
#' @param condition A condition required to be TRUE to set NA. Here, the condition
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
#'                  condition = ~.x == "N/A")
#'
#' replace_with_na(dat_ms,
#'               to_na = list(x = c(-99, -98),
#'                            y = c("N/A"),
#'                            z = c(-101)))
#'
#'
replace_with_na_if <- function(data, .predicate, condition) {
  test_if_dataframe(data)
  test_if_null(data)
  test_if_missing(data)
  purrr::modify_if(data, .predicate, ~ na_set(.x, condition))
}

# utility funs for replace_with_na_*  ------------------------------------------

#' @importFrom stats as.formula
create_mapper_na <- function(condition){
  glue::glue("~ {rlang::f_text(condition)} & !is.na(.x)") %>%
    as.formula() %>%
    purrr::as_mapper()
}

na_set <- function(vec, condition) {
  # modify this vector with this function, return NA
  purrr::modify_if(vec, create_mapper_na(condition) , ~ NA) %>%
    # flatten this out into a regular vector
    purrr::reduce(c)

  # na_set(aq_small$Ozone, ~ .x < 20)
}
