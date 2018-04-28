#' Identify if there are any missing values
#'
#' It is useful to search for any instances of missing values. By default
#'     handles atomic vectors without a class and NULL. It calls any(is.na(x))
#'     on objects with classes and for recursive = FALSE, on lists and
#'     pairlists. This means that you can get FALSE when you search through a
#'     list that contains missings. So, `any_na` is shorthand for
#'     `anyNA(x, recursive = TRUE)`.
#'
#'
#' @param x an R object to be tested
#'
#' @name any-na
#'
#' @examples
#'
#' # any_na() works recursively with lists
#' bag_o_lists <- list(A = 1:5,
#'                     B = c(NA, 5:8),
#'                     C = c("A","NA"))
#'
#' anyNA(bag_o_lists)
#' any_na(bag_o_lists)
#'
#' anyNA(airquality)
#' any_na(airquality)
#'
#'
#' @export
any_na <- function(x) anyNA(x, recursive = TRUE)

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

all_na <- function(x) all(is.na(x))

#' @rdname all-is-miss-complete
#' @export
all_complete <- function(x) all(!is.na(x))
