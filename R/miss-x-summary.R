#' Summarise the missingness in each variable
#'
#' Provide a summary for each variable of the number, percent missings, and
#'   cumulative sum of missings of the order of the variables. By default,
#'   it orders by the most missings in each variable.
#'
#' @param data a data.frame
#' @param order a logical indicating whether to order the result by `n_miss`.
#'     Defaults to TRUE. If FALSE, order of variables is the order input.
#' @param add_cumsum logical indicating whether or not to add the cumulative
#'   sum of missings to the data. This can be useful when exploring patterns
#'   of nonresponse. These are calculated as the cumulative sum of the missings
#'   in the variables as they are first presented to the function.
#' @param ... extra arguments
#'
#' @note `n_miss_cumsum` is calculated as the cumulative sum of missings in the
#'     variables in the order that they are given in the data when entering
#'     the function
#'
#' @return a tibble of the percent of missing data in each variable
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]() [n_complete]() [n_complete_row]() [n_miss]() [n_miss_row]() [pct_complete]() [pct_miss]() [prop_complete]() [prop_complete_row]() [prop_miss]()
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
miss_var_summary <- function(data,
                             order = FALSE,
                             add_cumsum = FALSE,
                             ...) {

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_var_summary")
}

#' @export
miss_var_summary.default <- function(data,
                                     order = TRUE,
                                     add_cumsum = FALSE,
                                     ...) {

  res <- purrr::map_df(data, n_miss) %>%
    tidyr::gather(key = "variable", value = "n_miss") %>%
    dplyr::mutate(pct_miss = (n_miss / nrow(data) * 100))

  if (add_cumsum) {
   res <- res %>% dplyr::mutate(n_miss_cumsum = cumsum(n_miss))
  }

  if (order) {
    return(dplyr::arrange(res, -n_miss))
  }

  return(res)


}

#' @export
miss_var_summary.grouped_df <- function(data,
                                        order = TRUE,
                                        add_cumsum = FALSE,
                                        ...) {

  group_by_fun(data,
               .fun = miss_var_summary,
               order = order,
               add_cumsum = add_cumsum)

}

#' Summarise the missingness in each case
#'
#' Provide a summary for each case in the data of the number, percent missings,
#'     and cumulative sum of missings of the order of the variables. By default,
#'     it orders by the most missings in each variable.
#'
#' @param data a data.frame
#' @param order a logical indicating whether or not to order the result by
#'     n_miss. Defaults to TRUE. If FALSE, order of cases is the order input.
#' @param ... extra arguments
#' @param add_cumsum logical indicating whether or not to add the cumulative
#'   sum of missings to the data. This can be useful when exploring patterns
#'   of nonresponse. These are calculated as the cumulative sum of the missings
#'   in the variables as they are first presented to the function.
#'
#' @return a tibble of the percent of missing data in each case.
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]() [n_complete]() [n_complete_row]() [n_miss]() [n_miss_row]() [pct_complete]() [pct_miss]() [prop_complete]() [prop_complete_row]() [prop_miss]()
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
miss_case_summary <- function(data,
                              order = TRUE,
                              add_cumsum = FALSE,
                              ...){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("miss_case_summary")
}

#' @export
miss_case_summary.default <- function(data,
                                      order = TRUE,
                                      add_cumsum = FALSE,
                                      ...){

  res <- data

  res[["pct_miss"]] <- rowMeans(is.na(res))*100
  res[["n_miss"]] <- as.integer(rowSums(is.na(res)))
  res[["case"]] <- seq_len(nrow(res))

  if (add_cumsum) {
    res[["n_miss_cumsum"]] <- cumsum(res[["n_miss"]])
    res <- dplyr::as_tibble(res)
    res <- dplyr::select(res,
                         case,
                         n_miss,
                         pct_miss,
                         n_miss_cumsum)
  }

  if (!add_cumsum) {
    res <- dplyr::as_tibble(res)

    res <- dplyr::select(res,
                         case,
                         n_miss,
                         pct_miss)

  }

  if (order) {
    return(
      dplyr::arrange(res, -n_miss) %>%
        new_mc_sum()
      )
  }

  if (!order) {
    return(new_mc_sum(res))
  }
}

#' @export
miss_case_summary.grouped_df <- function(data,
                                         order = TRUE,
                                         add_cumsum = FALSE,
                                         ...){

  group_by_fun(data,
               .fun = miss_case_summary,
               order = order,
               add_cumsum = add_cumsum)

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
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]() [n_complete]() [n_complete_row]() [n_miss]() [n_miss_row]() [pct_complete]() [pct_miss]() [prop_complete]() [prop_complete_row]() [prop_miss]()
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
miss_summary <- function(data, order = TRUE){

  test_if_null(data)

  test_if_dataframe(data)

  return(
    tibble::data_frame(
        miss_df_prop = prop_miss(data),
        miss_var_prop = prop_miss_var(data),
        miss_case_prop = prop_miss_case(data),
        miss_case_table = list(miss_case_table(data)),
        miss_var_table = list(miss_var_table(data)),
        miss_var_summary = list(miss_var_summary(data, order)),
        miss_case_summary = list(miss_case_summary(data, order))
      )
    )
  }
