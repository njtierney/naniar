#' Return the number of missing values
#'
#' Substitute for `sum(is.na(data))`
#'
#' @param x a vector
#'
#' @return numeric the number of missing values
#'
#' @export
#'
#' @examples
#'
#' n_na(airquality)
#' n_na(airquality$Ozone)
#'
n_na <- function(x){
  test_if_null(x)
  sum(is.na(x))
}

#' Return the number of complete values
#'
#' A complement to `n_na``
#'
#' @param x a vector
#'
#' @return numeric number of complete values
#'
#' @export
#'
#' @examples
#'
#' n_complete(airquality)
#' n_complete(airquality$Ozone)
#'
n_complete <- function(x){
  test_if_null(x)
  # number of total elements - number of missings
  length(is.na(x)) - n_na(x)

}
#' Return the proportion of missing values
#'
#' This is shorthand for `mean(is.na(x))`
#'
#' @param x vector or data.frame
#'
#' @return numeric the proportion of missing values in x
#'
#' @export
#'
#' @examples
#'
#' prop_na(airquality)
#' prop_na(airquality$Ozone)
#'
prop_na <- function(x){
  test_if_null(x)
  mean(is.na(x))
}

#' Return the percent of missing values
#'
#' This is shorthand for `mean(is.na(x)) * 100`
#'
#' @param x vector or data.frame
#'
#' @return numeric the percent of missing values in x
#'
#' @export
#'
#' @examples
#'
#' pct_na(airquality)
#' pct_na(airquality$Ozone)
#'
pct_na <- function(x){
  prop_na(x) * 100
}

#' Return the proportion of complete values
#'
#' The complement to `prop_na`
#'
#' @param x vector or data.frame
#'
#' @return numeric proportion of complete values
#'
#' @export
#'
#' @examples
#'
#' prop_complete(airquality)
#' prop_complete(airquality$Ozone)
#'
prop_complete <- function(x){
  test_if_null(x)
  # 1 - proportion of missings
  1 - mean(is.na(x))

}

#' Return the percent of complete values
#'
#' The complement to `pct_na`
#'
#' @param x vector or data.frame
#'
#' @return numeric percent of complete values
#'
#' @export
#'
#' @examples
#'
#' pct_complete(airquality)
#' pct_complete(airquality$Ozone)
#'
pct_complete <- function(x){
  test_if_null(x)
  prop_complete(x) * 100

}
