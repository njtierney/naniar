#' Return the number of missing values
#'
#' Substitute for \code{sum(is.na(data))}
#'
#' @param x a vector
#'
#' @return numeric the number of missing values
#'
#' @export
#'
#' @examples
#'
#' n_miss(airquality)
#' n_miss(airquality$Ozone)
#'
n_miss <- function(x){
  sum(is.na(x))
}

#' Return the number of complete values
#'
#' A complement to `n_miss``
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

  # number of total elements - number of missings
  length(is.na(x)) - n_miss(x)

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
#' prop_miss(airquality)
#' prop_miss(airquality$Ozone)
#'
prop_miss <- function(x){
  mean(is.na(x))
}

#' Return the proportion of complete values
#'
#' The complement to `prop_miss`
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

  # 1 - proportion of missings
  1 - mean(is.na(x))

}
