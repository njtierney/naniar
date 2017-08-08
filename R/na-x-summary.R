#' Summarise the missingness in each variable
#'
#' Provide a summary for each variable of the number and percent missings,
#'   ordering by the most missings in each variable.
#'
#' @param data a data.frame
#' @param ... extra arguments
#'
#' @return a tibble of the percent of missing data in each variable
#' @export
#'
#' @examples
#'
#' na_var_summary(airquality)
#'
#' # works with group_by from dplyr
#' library(dplyr)
#' airquality %>%
#'   group_by(Month) %>%
#'   na_var_summary()
#'
#' @export
na_var_summary <- function(data, ...) {

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("na_var_summary")
}

#' @export
na_var_summary.default <- function(data, ...) {
  purrr::map_df(data, n_na) %>%
    tidyr::gather(key = "variable", value = "n_missing") %>%
    dplyr::mutate(percent = (n_missing / nrow(data) * 100)) %>%
    dplyr::arrange(-n_missing)
}

#' @export
na_var_summary.grouped_df <- function(data, ...) {

  group_by_fun(data, .fun = na_var_summary)

}

#' Summarise the missingness in each case
#'
#' Return for each case the number and percent of missing values, ordered by the
#' most number of missings.
#'
#' @param data a data.frame
#'
#' @return a tibble of the percent of missing data in each case.
#' @export
#'
#' @examples
#'
#' # works with group_by from dplyr
#' library(dplyr)
#' airquality %>%
#'   group_by(Month) %>%
#'   na_case_summary()
#'
#' na_case_summary(airquality)
#'
na_case_summary <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("na_case_summary")
}

#' @export
na_case_summary.default <- function(data){

    purrrlyr::by_row(.d = data,
                            ..f = function(x) (mean(is.na(x)) * 100),
                            .collate = "row",
                            .to = "percent") %>%
      purrrlyr::by_row(.d = .,
                       ..f = function(x) (sum(is.na(x))),
                       .collate = "row",
                       .to = "n_missing") %>%
      dplyr::mutate(case = 1:nrow(data)) %>%
      dplyr::select(case,
                    n_missing,
                    percent) %>%
      dplyr::arrange(-n_missing)

}

#' @export
na_case_summary.grouped_df <- function(data){

  group_by_fun(data, .fun = na_case_summary)

}

#' Collate summary measures from naniar into one tibble
#'
#' `na_summary` performs all of the missing data helper summaries and puts
#'   them into lists within a tibble
#'
#' @param data a dataframe
#'
#' @return a tibble of missing data summaries
#' @export
#'
#' @examples
#'
#' s_miss <- na_summary(airquality)
#' s_miss$na_df_prop
#' s_miss$na_case_table
#' # etc, etc, etc.
#'
#' library(dplyr)
#' s_na_group <- group_by(airquality, Month) %>% na_summary()
#' s_na_group$na_df_prop
#' s_na_group$na_case_table
#' # etc, etc, etc.
#'
#'
na_summary <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  return(
    tibble::data_frame(
        na_df_prop = prop_na(data),
        na_var_prop = na_var_prop(data),
        na_case_prop = na_case_prop(data),
        na_case_table = list(na_case_table(data)),
        na_var_table = list(na_var_table(data)),
        na_var_summary = list(na_var_summary(data)),
        na_case_summary = list(na_case_summary(data)),
        na_var_cumsum = list(na_var_cumsum(data)),
        na_case_cumsum = list(na_case_cumsum(data))
      )
    )
  }
