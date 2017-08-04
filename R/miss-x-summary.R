#' Summarise the missingness in each variable
#'
#' Provide a data_frame containing the variable names, the number of missing values, in each variable, and the percent of missing values in each variable.
#'
#' @param data a dataframe
#' @param ... extra arguments
#'
#' @return a data_frame of the percent of missing data in each variable
#' @export
#'
#' @examples
#'
#' miss_var_summary(airquality)
#' # works with group_by from dplyr
#' library(dplyr)
#' airquality %>%
#' group_by(Month) %>%
#' miss_var_summary()
#'
#' @export
miss_var_summary <- function(data, ...) {
  if (is.null(data)) {
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  }

  if (!inherits(data, "data.frame")) {
    stop("Input must inherit from data.frame", call. = FALSE)
  }

  UseMethod("miss_var_summary")
}

#' @export
miss_var_summary.default <- function(data, ...) {
  purrr::map_df(data, n_miss) %>%
    tidyr::gather(key = "variable", value = "n_missing") %>%
    dplyr::mutate(percent = (n_missing / nrow(data) * 100)) %>%
    dplyr::arrange(-n_missing)
}

#' @export
miss_var_summary.grouped_df <- function(data, ...) {

  group_by_fun(data, .fun = miss_var_summary)

}

#' Summarise the missingness in each case
#'
#' Provide a data_frame containing each case (row), the number of missing
#' values in each case, and the percent of missing values in each case.
#'
#' @param data a dataframe
#'
#' @return a data_frame of the percent of missing data in each case
#' @export
#'
#' @examples
#'
#' miss_case_summary(airquality)
#'
miss_case_summary <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_case_summary")
}

#' @export

miss_case_summary.default <- function(data){

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

miss_case_summary.grouped_df <- function(data){

  group_by_fun(data, .fun = miss_case_summary)

}


#' Collate summary measures from naniar into one tibble
#'
#' \code{miss_summary} performs all of the missing data helper summaries and puts them into a list. Perhaps in the future this can all be some sort of nested dataframe?
#'
#' @param data a dataframe
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' s_miss <- miss_summary(airquality)
#' s_miss$miss_df_prop
#' s_miss$miss_case_table
#' # etc, etc, etc.
#'
miss_summary <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  return(
    tibble::data_frame(
        miss_df_prop = prop_miss(data),
        miss_var_prop = miss_var_prop(data),
        miss_case_prop = miss_case_prop(data),
        miss_case_table = list(miss_case_table(data)),
        miss_var_table = list(miss_var_table(data)),
        miss_var_summary = list(miss_var_summary(data)),
        miss_case_summary = list(miss_case_summary(data))
      )
    )
  }
