#' Replace values with missings
#'
#' This function is deprecated, please see [replace_with_na()].
#'
#' @param data A data.frame
#' @param to_na A named list given the NA to replace values
#' @param ... additional arguments for methods.
#'
#' @return values replaced by NA
#' @export
#'
replace_to_na <- function(data, to_na = list(), ...){
  .Deprecated("replace_with_na")
  UseMethod("replace_to_na")
}

#' @export
replace_to_na.data.frame <- function(data, to_na = list(), ...){

  stopifnot(is.list(to_na))

  for (var in names(to_na)) {
    data[[var]][data[[var]] %in% unlist(to_na[[var]])] <- NA
  }
  data
}
