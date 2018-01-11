# utility functions for replace_to_na_* family. -------------------------------

create_mapper_na <- function(.p){
  glue::glue("~ {rlang::f_text(.p)} & !is.na(.x)") %>%
    as.formula() %>%
    purrr::as_mapper()
}

na_set <- function(vec, .p) {
  # modify this vector with this function, return NA
  purrr::modify_if(vec, create_mapper_na(.p) , ~ NA) %>%
    # flatten this out into a regular vector
    purrr::reduce(c)
}

# na_set(aq_small$Ozone, ~ .x < 20)

#' Replace all values with NA where a certain condition is met
#'
#' This function takes a dataframe and replaces all values that meet the
#'   condition specified as an NA value, following a special syntax.
#'
#' @param data A dataframe
#' @param .p the condition required to be TRUE to set NA - the conditions are
#'   referred to as ~.x {condition} - e.g., "~.x < 20" means "where the
#'   value is less than 20."
#'
#' @examples
#' # Replace all values less than 20
#' replace_to_na_where(airqaulity, ~ .x < 20)
#'
#' # where works with functions
#' replace_to_na_where(airquality, ~ sqrt(.x) < 5)
#'
#' @export
replace_to_na_where <- function(data, .p) {
  purrr::map_df(data, ~ na_set(.x, .p) )
}

# Future work
  # replace_to_na_where(airquality, . < 20)
  # replace_to_na_where(airquality, x < 20)
  # replace_to_na_where(airquality, function(x) mean(x) < 20)


#' Replace specified variables with NA where a certain condition is met
#'
#' @param data dataframe
#' @param .vars The variables to refer to
#' @param .p the condition required to be TRUE to set NA - the conditions are
#'   referred to as ~.x {condition} - e.g., "~.x < 20" means "where the
#'   value is less than 20."
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#' replace_to_na_at(airquality, .vars = c("Wind", "Ozone"), .p = ~ .x < 20)
replace_to_na_at <- function(data, .vars, .p) {
  test_if_dataframe(data)
  test_if_null(data)
  test_if_missing(data)
  purrr::modify_at(data, .vars, ~ na_set(.x, .p))
}

# Future work
   # use of `vars` - `vars(var1, var2)`
   # replace_to_na_at(tbl = airquality, .at = vars(Wind, Ozone), ~ .x < 20)

#' Replace values with NA based on some condition, for variables that meet some predicate
#'
#' @param data dataframe
#' @param .predicate a predicate function to be applied to the columns or a logical vector
#' @param .funs the condition required to be TRUE to set NA - the conditions are
#'   referred to as ~.x {condition} - e.g., "~.x < 20" means "where the
#'   value is less than 20."
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' replace_to_na_if(tbl = airquality, is.numeric, ~ .x < 20)
#'
replace_to_na_if <- function(data, .predicate, .funs) {
  test_if_dataframe(data)
  test_if_null(data)
  test_if_missing(data)
  purrr::modify_if(data, .predicate, ~ na_set(.x, .funs))
}


