#' Tabulate missings in cases.
#'
#' Provide a tidy table of the number of cases with 0, 1, 2, up to n, missing
#' values and the proportion of the number of cases those cases make up.
#'
#' @param data a dataframe
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' na_case_table(airquality)
#' library(dplyr)
#' airquality %>%
#'   group_by(Month) %>%
#'   na_case_table()
#'
na_case_table <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("na_case_table")

}

#' @export
na_case_table.default <- function(data){

  purrrlyr::by_row(.d = data,
                   # how many are missing in each row?
                   ..f = ~n_na(.),
                   .collate = "row",
                   .to = "n_missing_in_case") %>%
    dplyr::group_by(n_missing_in_case) %>%
    dplyr::tally() %>%
    dplyr::mutate(percent = (n / nrow(data) * 100)) %>%
    dplyr::rename(n_cases = n)

}


#' @export
na_case_table.grouped_df <- function(data){

  group_by_fun(data, .fun = na_case_table)

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
#' @export
#'
#' @examples
#'
#' na_var_table(airquality)
#'
#' library(dplyr)
#' airquality %>%
#'   group_by(Month) %>%
#'   na_var_table()
#'
na_var_table <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("na_var_table")

}

#' @export

na_var_table.default <- function(data){

  purrr::map_df(data, ~sum(is.na(.))) %>%
    tidyr::gather(key = "variable",
                  value = "n_missing_in_var") %>%
    dplyr::group_by(n_missing_in_var) %>%
    dplyr::tally() %>%
    dplyr::rename(n_vars = n) %>%
    dplyr::mutate(percent = (n_vars / ncol(data) * 100))

}

#' @export

na_var_table.grouped_df <- function(data){

  group_by_fun(data, .fun = na_var_table)

}
