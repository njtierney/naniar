#' Tabulate missings in cases.
#'
#' Provide a tidy table of the number of cases with 0, 1, 2, up to n, missing
#' values and the proportion of the number of cases those cases make up.
#'
#' @param data a dataframe
#'
#' @return a dataframe
#'
#' @seealso [miss_case_pct]() [miss_case_prop]() [miss_prop_summary()] [miss_case_summary]() [miss_summary]() [miss_var_pct]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]()
#'
#' @export
#'
#' @examples
#'
#' miss_case_table(airquality)
#' library(dplyr)
#' airquality %>%
#'   group_by(Month) %>%
#'   miss_case_table()
#'
miss_case_table <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_case_table")

}

#' @export
miss_case_table.default <- function(data){

  data[["n_miss_in_case"]] <- n_miss_row(data)

  data %>%
    dplyr::group_by(n_miss_in_case) %>%
    dplyr::tally() %>%
    dplyr::mutate(pct_miss = (n / nrow(data) * 100)) %>%
    dplyr::rename(n_cases = n)

}


#' @export
miss_case_table.grouped_df <- function(data){

  group_by_fun(data, .fun = miss_case_table)

}

#' Tabulate the missings in the variables
#'
#' Provide a tidy table of the number of variables with 0, 1, 2, up to n,
#'   missing values and the proportion of the number of variables those
#'   variables make up.
#'
#' @param data a dataframe
#'
#' @return a dataframe
#'
#' @seealso [miss_case_pct]() [miss_case_prop]() [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_pct]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]()
#'
#' @export
#'
#' @examples
#'
#' miss_var_table(airquality)
#'
#' library(dplyr)
#' airquality %>%
#'   group_by(Month) %>%
#'   miss_var_table()
#'
miss_var_table <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_var_table")

}

#' @export

miss_var_table.default <- function(data){

  purrr::map_df(data, ~n_miss(.)) %>%
    tidyr::gather(key = "variable",
                  value = "n_miss_in_var") %>%
    dplyr::group_by(n_miss_in_var) %>%
    dplyr::tally() %>%
    dplyr::rename(n_vars = n) %>%
    dplyr::mutate(pct_miss = (n_vars / ncol(data) * 100))

}

#' @export

miss_var_table.grouped_df <- function(data){

  group_by_fun(data, .fun = miss_var_table)

}
