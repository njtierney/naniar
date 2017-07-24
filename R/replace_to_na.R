#' Replace values with missings
#'
#' Specify variables and their values that you want to convert to missing values.
#' This is a complement to `tidyr::replace_na`.
#'
#' @param data A data.frame
#' @param to_na A named list given the NA to replace values
#' @param ... additional arguments for methods.
#'
#' @return values replaced by NA
#' @export
#'
#' @examples
#'
#'dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#'                          1,   "A",   -100,
#'                          3,   "N/A", -99,
#'                          NA,  NA,    -98,
#'                          -99, "E",   -101,
#'                          -98, "F",   -1)
#'
#'replace_to_na(dat_ms,
#'              to_na = list(x = -99))
#'
#'replace_to_na(dat_ms,
#'              to_na = list(x = -98))
#'
#'replace_to_na(dat_ms,
#'              to_na = list(x = c(-99, -98)))
#'
#'replace_to_na(dat_ms,
#'              to_na = list(x = c(-99, -98),
#'                           y = c("N/A")))
#'
#'replace_to_na(dat_ms,
#'              to_na = list(x = c(-99, -98),
#'                           y = c("N/A"),
#'                           z = c(-101)))
replace_to_na <- function(data, replace = list(), ...){
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

#
# dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#                           1,   "A",   -100,
#                           3,   "N/A", -99,
#                           NA,  NA,    -98,
#                           -99, "E",   -101,
#                           -98, "F",   -1)
#
# replace_to_na_all <- function(data, to_na = NULL, ...){
#
#   for (var in names(to_na)) {
#     print(data[[var]] %in% to_na)
#     data[[var]][data[[var]] %in% to_na] <- NA
#   }
#   data
# }
#
# # dat_ms[["x"]][dat_ms[["x"]] %in% -99]
#
# replace_to_na_all(dat_ms,
#                   to_na = -99)
#
# replace_to_na(dat_ms,
#               to_na = list(x = -98))
#
# replace_to_na(dat_ms,
#               to_na = list(x = c(-99, -98)))
#
# replace_to_na(dat_ms,
#               to_na = list(x = c(-99, -98),
#                            y = c("N/A")))
#
# replace_to_na(dat_ms,
#               to_na = list(x = c(-99, -98),
#                            y = c("N/A"),
#                            z = c(-101)))
