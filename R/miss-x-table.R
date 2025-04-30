#' Tabulate missings in cases.
#'
#' Provide a tidy table of the number of cases with 0, 1, 2, up to n, missing
#' values and the proportion of the number of cases those cases make up.
#'
#' @param data a dataframe
#'
#' @return a dataframe
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()] [n_complete()] [n_complete_row()] [n_miss()] [n_miss_row()] [pct_complete()] [pct_miss()] [prop_complete()] [prop_complete_row()] [prop_miss()]
#'
#' @export
#'
#' @examples
#'
#' miss_case_table(airquality)
#' \dontrun{
#' library(dplyr)
#' airquality %>%
#'   group_by(Month) %>%
#'   miss_case_table()
#' }
miss_case_table <- function(data) {
  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_case_table")
}

#' @export
miss_case_table.default <- function(data) {
  data[["n_miss_in_case"]] <- n_miss_row(data)

  data %>%
    dplyr::group_by(n_miss_in_case) %>%
    dplyr::tally() %>%
    dplyr::mutate(pct_cases = (n / nrow(data) * 100)) %>%
    dplyr::rename(n_cases = n)
}


#' @export
miss_case_table.grouped_df <- function(data) {
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
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()] [n_complete()] [n_complete_row()] [n_miss()] [n_miss_row()] [pct_complete()] [pct_miss()] [prop_complete()] [prop_complete_row()] [prop_miss()]
#'
#'
#' @examples
#'
#' miss_var_table(airquality)
#' \dontrun{
#' library(dplyr)
#' airquality %>%
#'   group_by(Month) %>%
#'   miss_var_table()
#' }
#' @export
miss_var_table <- function(data) {
  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_var_table")
}

#' @export

miss_var_table.default <- function(data) {
  miss_var_summary(data) %>%
    dplyr::rename(n_miss_in_var = n_miss) %>%
    dplyr::select(-pct_miss) %>%
    dplyr::group_by(n_miss_in_var) %>%
    dplyr::tally() %>%
    dplyr::rename(n_vars = n) %>%
    dplyr::mutate(pct_vars = (n_vars / ncol(data) * 100))
}

#' @export

miss_var_table.grouped_df <- function(data) {
  group_by_fun(data, .fun = miss_var_table)
}
