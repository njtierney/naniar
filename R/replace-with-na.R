#' Replace values with missings
#'
#' Specify variables and their values that you want to convert to missing values.
#'   This is a complement to `tidyr::replace_na`.
#'
#' @param data A data.frame
#' @param replace A named list given the NA to replace values for each column
#' @param ... additional arguments for methods. Currently unused
#' @seealso [replace_with_na()] [replace_with_na_all()] [replace_with_na_at()] [replace_with_na_if()]
#'
#' @return Dataframe with values replaced by NA.
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
#'replace_with_na(dat_ms,
#'                replace = list(x = -99))
#'
#'replace_with_na(dat_ms,
#'              replace = list(x = c(-99, -98)))
#'
#'replace_with_na(dat_ms,
#'              replace = list(x = c(-99, -98),
#'                           y = c("N/A"),
#'                           z = c(-101)))
replace_with_na <- function(data, replace = list(), ...) {
  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("replace_with_na")
}

#' @export
replace_with_na.data.frame <- function(data, replace = list(), ...) {
  if (!is.list(replace)) {
    cli::cli_abort(
      c(
        "{.arg replace} must be a list",
        "I see that replace has class {.cls {class(replace)}}",
        " and {.code typeof} {typeof(replace)}",
        "see {.code ?replace_with_na} for more details"
      )
    )
  }

  new_replace <- replace[names(replace) %in% names(data)]

  missing <- setdiff(names(replace), names(new_replace))

  if (length(missing) > 0) {
    cli::cli_warn(
      c(
        "Missing from data: {.arg {missing}}"
      )
    )
  }

  for (var in names(new_replace)) {
    data[[var]][data[[var]] %in% unlist(new_replace[[var]])] <- NA
  }
  data
}
