#' Identify if there are any missing or complete values
#'
#' It is useful to search for any instances of missing or complete values. There
#'     Are two functions that do this in `naniar` - `any_miss` and it's alias
#'     `any_na`. These bother under the hood call `anyNA`. `any_complete` is
#'     the complement to `any_miss` - it returns TRUE if there are any complete values.
#'
#'
#' @param x an R object to be tested
#'
#' @name any-na
#' @seealso [all_miss()] [all_complete]
#'
#' @examples
#'
#' anyNA(airquality)
#' any_na(airquality)
#' any_miss(airquality)
#' any_complete(airquality)
#'
#'
#' @export
any_na <- function(x) anyNA(x)

#' @rdname any-na
#' @export
any_miss <- any_na

#' @rdname any-na
#' @export
any_complete <- function(x) any(complete.cases(x))

#' Identify if all values are missing or complete
#'
#' This is shorthand for `all(is.na(x))` and `all(!is.na(x))`
#'
#' @param x an R object to be tested.
#'
#' @examples
#'
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
#' @name all-is-miss-complete
#' @export

all_na <- function(x){
  # if there are no missings, then there cannot be all missings
  if (!anyNA(x)) {
    return(FALSE)
  }

  return(all(is.na(x)))

}

#' @rdname all-is-miss-complete
#' @export
all_miss <- all_na

#' @rdname all-is-miss-complete
#' @export
all_complete <- function(x) !anyNA(x)
