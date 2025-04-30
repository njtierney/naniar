#' Identify if there are any or all missing or complete values
#'
#' @description It is useful when exploring data to search for cases where
#'   there are **any** or **all** instances of missing or complete values. For
#'   example, these can help you identify and potentially remove or keep
#'   columns in a data frame that are all missing, or all complete.
#'
#'   For the **any** case, we provide two functions: `any_miss` and
#'   `any_complete`. Note that `any_miss` has an alias, `any_na`. These both
#'   under the hood call `anyNA`. `any_complete` is the complement to
#'   `any_miss` - it returns TRUE if there are any complete values. Note
#'   that in a dataframe `any_complete` will look for complete cases, which
#'   are complete rows, which is different to complete variables.
#'
#'   For the **all** case, there are two functions: `all_miss`, and
#'   `all_complete`.
#'
#' @param x an object to explore missings/complete values
#'
#' @name any-all-na-complete
#' @seealso [all_miss()] [all_complete]
#'
#' @examples
#'
#' # for vectors
#' misses <- c(NA, NA, NA)
#' complete <- c(1, 2, 3)
#' mixture <- c(NA, 1, NA)
#'
#' all_na(misses)
#' all_na(complete)
#' all_na(mixture)
#' all_complete(misses)
#' all_complete(complete)
#' all_complete(mixture)
#'
#' any_na(misses)
#' any_na(complete)
#' any_na(mixture)
#'
#' # for data frames
#' all_na(airquality)
#' # an alias of all_na
#' all_miss(airquality)
#' all_complete(airquality)
#'
#' any_na(airquality)
#' any_complete(airquality)
#'
#' # use in identifying columns with all missing/complete
#'
#' library(dplyr)
#' # for printing
#' aq <- as_tibble(airquality)
#' aq
#' # select variables with all missing values
#' aq %>% select(where(all_na))
#' # there are none!
#' #' # select columns with any NA values
#' aq %>% select(where(any_na))

#' # select only columns with all complete data
#' aq %>% select(where(all_complete))
#'
#' # select columns where there are any complete cases (all the data)
#' aq %>% select(where(any_complete))
#'
#' @export
any_na <- function(x) anyNA(x)

#' @rdname any-all-na-complete
#' @export
any_miss <- any_na

#' @rdname any-all-na-complete
#' @export
any_complete <- function(x) any(complete.cases(x))

#' @rdname any-all-na-complete
#' @export
all_na <- function(x) {
  # if there are no missings, then there cannot be all missings
  if (!anyNA(x)) {
    return(FALSE)
  }

  return(all(is.na(x)))
}

#' @rdname any-all-na-complete
#' @export
all_miss <- all_na

#' @rdname any-all-na-complete
#' @export
all_complete <- function(x) all(complete.cases(x))
