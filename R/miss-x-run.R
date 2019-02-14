#' Find the number of missing and complete values in a single run
#'
#' It us useful to find the number of missing values that occur in a single run.
#'    The function, `miss_var_run()`, returns a dataframe with the column names
#'    "run_length" and "is_na", which describe the length of the run, and
#'    whether that run describes a missing value.
#'
#' @param data data.frame
#'
#' @param var a bare variable name
#'
#' @return dataframe with column names "run_length" and "is_na", which describe
#'     the length of the run, and whether that run describes a missing value.
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()] [n_complete()] [n_complete_row()] [n_miss()] [n_miss_row()] [pct_complete()] [pct_miss()] [prop_complete()] [prop_complete_row()] [prop_miss()]
#'
#' @export
#'
#' @examples
#'
#' miss_var_run(pedestrian, hourly_counts)
#'
#' library(dplyr)
#'
#' # find the number of runs missing/complete for each month
#'
#' pedestrian %>%
#'   group_by(month) %>%
#'   miss_var_run(hourly_counts)
#'
#' library(ggplot2)
#'
#' # explore the number of missings in a given run
#' miss_var_run(pedestrian, hourly_counts) %>%
#'   filter(is_na == "missing") %>%
#'   count(run_length) %>%
#'   ggplot(aes(x = run_length,
#'              y = n)) +
#'       geom_col()
#'
#' # look at the number of missing values and the run length of these.
#' miss_var_run(pedestrian, hourly_counts) %>%
#'   ggplot(aes(x = is_na,
#'              y = run_length)) +
#'       geom_boxplot()
#'
#'# using group_by
#'  pedestrian %>%
#'    group_by(month) %>%
#'    miss_var_run(hourly_counts)
#'
#'
miss_var_run <- function(data, var){

  test_if_null(data)

  test_if_missing(var)

  test_if_dataframe(data)

  UseMethod("miss_var_run")

}

#' @export
miss_var_run.default <- function(data, var){

  var <- rlang::enquo(var)

  data_pull <-  data %>% dplyr::pull(!!var)

    tibble::as_tibble(c(rle(is.na(data_pull)))) %>%
    dplyr::rename(run_length = lengths,
                  is_na = values) %>%
      dplyr::mutate(is_na = dplyr::if_else(is_na == TRUE,
                                           true = "missing",
                                           false = "complete"))
    # also look into `label_na`
    # naniar::is_na(TRUE)

}

#' @export
miss_var_run.grouped_df <- function(data,var){

  var <- rlang::enquo(var)

  tidyr::nest(data) %>%
    dplyr::mutate(data = purrr::map(data,
                                    var = !!var,
                                    .f = miss_var_run)) %>%
    tidyr::unnest()

}

# library(imputeTS)

# How can I improve the summaries below?
# statsNA(tsNH4)

# [1] "Stats for Bins"
# [1] "  Bin 1 (1138 values from 1 to 1138) :      233 NAs (20.5%)"
# [1] "  Bin 2 (1138 values from 1139 to 2276) :      433 NAs (38%)"
# [1] "  Bin 3 (1138 values from 2277 to 3414) :      135 NAs (11.9%)"
# [1] "  Bin 4 (1138 values from 3415 to 4552) :      82 NAs (7.21%)"
#
# [1] "Longest NA gap (series of consecutive NAs)"
# [1] "157 in a row"

# [1] "Most frequent gap size (series of consecutive NA series)"
# [1] "1 NA in a row (occuring 68 times)"

# [1] "Gap size accounting for most NAs"
# [1] "157 NA in a row (occuring 1 times, making up for overall 157 NAs)"

# [1] "Overview NA series"
# [1] "  1 NA in a row: 68 times"
# [1] "  2 NA in a row: 26 times"
# [1] "  3 NA in a row: 16 times"
# [1] "  4 NA in a row: 10 times"
# [1] "  5 NA in a row: 8 times"
# [1] "  6 NA in a row: 4 times"
# [1] "  7 NA in a row: 2 times"
# [1] "  8 NA in a row: 3 times"
# [1] "  9 NA in a row: 2 times"
# [1] "  10 NA in a row: 1 times"
# [1] "  11 NA in a row: 1 times"
# [1] "  12 NA in a row: 2 times"
# [1] "  14 NA in a row: 1 times"
# [1] "  16 NA in a row: 1 times"
# [1] "  17 NA in a row: 1 times"
# [1] "  21 NA in a row: 1 times"
# [1] "  25 NA in a row: 1 times"
# [1] "  26 NA in a row: 1 times"
# [1] "  27 NA in a row: 1 times"
# [1] "  32 NA in a row: 1 times"
# [1] "  42 NA in a row: 2 times"
# [1] "  91 NA in a row: 1 times"
# [1] "  157 NA in a row: 1 times"

#
# n_miss(tsNH4)
# n_complete(tsNH4)
# prop_miss(tsNH4)
# prop_complete(tsNH4)
#
# # Stats for bins?
#
# miss_ts_run(tsNH4)
# miss_ts_summary(dat_ts = tsNH4, period = 100)
