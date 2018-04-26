#' Summarise the missingness in each variable
#'
#' Provide a summary for each variable of the number and percent missings,
#'   ordering by the most missings in each variable.
#'
#' @param data a data.frame
#' @param order a logical indicating whether or not to order the result by n_miss. TRUE orders from largest to smallest n_miss, and FALSE orders by order provided by the data.
#' @param ... extra arguments
#'
#' @return a tibble of the percent of missing data in each variable
#'
#' @seealso [miss_case_pct]() [miss_case_prop]() [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_pct]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_table]()
#'
#' @export
#'
#' @examples
#'
#' miss_var_summary(airquality)
#' miss_var_summary(oceanbuoys, order = TRUE)
#'
#' # works with group_by from dplyr
#' library(dplyr)
#' airquality %>%
#'   group_by(Month) %>%
#'   miss_var_summary()
#'
#' @export
miss_var_summary <- function(data, order = FALSE, ...) {

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_var_summary")
}

#' @export
miss_var_summary.default <- function(data, order = FALSE, ...) {
  res <- purrr::map_df(data, n_miss) %>%
    tidyr::gather(key = "variable", value = "n_miss") %>%
    dplyr::mutate(pct_miss = (n_miss / nrow(data) * 100),
                  n_miss_cumsum = cumsum(n_miss))
  if (order) {
    return(dplyr::arrange(res, -n_miss))
  } else {
    return(res)
  }
}

#' @export
miss_var_summary.grouped_df <- function(data, order = FALSE, ...) {

  group_by_fun(data, .fun = miss_var_summary, order = order)

}

#' Summarise the missingness in each case
#'
#' Return for each case the number and percent of missing values, ordered by the
#' most number of missings.
#'
#' @param data a data.frame
#' @param order a logical indicating whether or not to order the result by n_miss. TRUE orders from largest to smallest n_miss, and FALSE orders by order provided by the data.
#' @param ... extra arguments
#'
#' @return a tibble of the percent of missing data in each case.
#'
#' @seealso [miss_case_pct]() [miss_case_prop]() [miss_prop_summary()] [miss_case_table]() [miss_summary]() [miss_var_pct]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]()
#'
#' @export
#'
#' @examples
#'
#' # works with group_by from dplyr
#' library(dplyr)
#' airquality %>%
#'   group_by(Month) %>%
#'   miss_case_summary()
#'
#' miss_case_summary(airquality)
#'
miss_case_summary <- function(data, order = FALSE, ...){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_case_summary")
}

#' @export
miss_case_summary.default <- function(data, order = FALSE, ...){

  res <- data

  res[["pct_miss"]] <- rowMeans(is.na(res))*100
  res[["n_miss"]] <- as.integer(rowSums(is.na(res)))
  res[["case"]] <- seq_len(nrow(res))
  res[["n_miss_cumsum"]] <- cumsum(res[["n_miss"]])

  res <- dplyr::as_tibble(res)

  res <- dplyr::select(res,
                       case,
                       n_miss,
                       pct_miss,
                       n_miss_cumsum)

  if (order) {
    return(dplyr::arrange(res, -n_miss))
  } else {
    return(res)
  }
}

#' @export
miss_case_summary.grouped_df <- function(data, order = FALSE, ...){

  group_by_fun(data, .fun = miss_case_summary, order = order)

}

#' Collate summary measures from naniar into one tibble
#'
#' `miss_summary` performs all of the missing data helper summaries and puts
#'   them into lists within a tibble
#'
#' @param data a dataframe
#' @param order whether or not to order the result by n_miss
#' @param ... extra arguments
#'
#' @return a tibble of missing data summaries
#'
#' @seealso [miss_case_pct]() [miss_case_prop]() [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_var_pct]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]()
#'
#' @export
#'
#' @examples
#'
#' s_miss <- miss_summary(airquality)
#' s_miss$miss_df_prop
#' s_miss$miss_case_table
#' s_miss$miss_var_summary
#' # etc, etc, etc.
#'
#' library(dplyr)
#' s_miss_group <- group_by(airquality, Month) %>% miss_summary()
#' s_miss_group$miss_df_prop
#' s_miss_group$miss_case_table
#' # etc, etc, etc.
#'
#'
miss_summary <- function(data, order = FALSE){

  test_if_null(data)

  test_if_dataframe(data)

  return(
    tibble::data_frame(
        miss_df_prop = prop_miss(data),
        miss_var_prop = miss_var_prop(data),
        miss_case_prop = miss_case_prop(data),
        miss_case_table = list(miss_case_table(data)),
        miss_var_table = list(miss_var_table(data)),
        miss_var_summary = list(miss_var_summary(data, order)),
        miss_case_summary = list(miss_case_summary(data, order))
      )
    )
  }
