#' Summarise the number of missings for a given repeating span on a variable
#'
#' To summarise the missing values in a time series object it can be useful to
#'     calculate the number of missing values in a given time period.
#'     `miss_var_span` takes a data.frame object, a variable, and a `span_every`
#'     argument and returns a dataframe containing the number of missing values
#'     within each span.
#'
#' @param data data.frame
#' @param var variable of interest. Currently just one variable
#' @param span_every integer describing the length of the span to be explored
#'
#' @return dataframe with variables `n_miss`, `n_complete`, `prop_miss`, and
#'     `prop_complete`, which describe the number, or proportion of missing or
#'     complete values within that given time span.
#'
#' @export
#'
#' @examples
#'
#' miss_var_span(data = pedestrian,
#'               var = hourly_counts,
#'               span_every = 168)
#'
miss_var_span <- function(data,
                          var,
                          span_every){

  var_enquo <- rlang::enquo(var)
  # var_quo <- rlang::quo(var)

  dat_ts_summary <- dplyr::select(data,!!!var_enquo)
  # dat_ts_summary <- dplyr::select(data,!!!var_quo)

  dat_ts_summary %>%
    # need to make add_span_counter respect grouping structure, somehow
    add_span_counter(span_size = span_every) %>%
    dplyr::group_by(span_counter) %>%
    dplyr::tally(is.na(!!!var_enquo)) %>%
    # dplyr::tally(is.na(!!var_quo)) %>%
    dplyr::rename(n_miss = n) %>%
    dplyr::mutate(n_complete = span_every - n_miss,
                  prop_miss = n_miss / span_every,
                  prop_complete = 1 - prop_miss)

}

# some alternative names
# miss_var_interval
# miss_var_stretch
# miss_var_term
# miss_var_span
# miss_var_phase
# miss_var_run
# miss_var_period
# miss_var_cycle
# miss_var_window

#' Return the number of missing or complete values in a single run
#'
#' In time series it can be useful to determine the number of missing values
#'     that occur in a single run. This function `miss_var_run` returns a
#'     dataframe with the column names "run_length" and "is_na", which describe
#'     the length of the run, and whether that run describes a missing value
#'
#' @param data data.frame
#' @param var a bare variable name
#'
#' @return dataframe with column names "run_length" and "is_na", which describe
#'     the length of the run, and whether that run describes a missing value.
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' library(dplyr)
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
#'
miss_var_run <- function(data, var){

  var_enquo <- rlang::enquo(var)

  # grouping <- rlang::quos(...)

  data_pull <-  data %>%
    dplyr::pull(!!var_enquo)
    # dplyr::group_by(!!!grouping) %>%
    tibble::as_tibble(c(rle(is.na(data_pull)))) %>%
    dplyr::rename(run_length = lengths,
                  is_na = values) %>%
      dplyr::mutate(is_na = dplyr::if_else(is_na == TRUE,
                                           true = "missing",
                                           false = "complete"))
    # also look into `label_na`
    # naniar::is_na(TRUE)

}

# Need to make this work for:
# multiple time series (mts), a facet for each variable.
# regular data.frames that contain time series / time like data.
# detect a date/time object, perform something special
# user could also provide the date/time variable of interest
# example mts objects - fpp2::arrivals


# n_period # how many periods in your time series do you want?
# size_period # how long are the periods in your time series.
# this could also be a vector
# size_period = c(3,10,100) OR
# size_period = c(100) OR
# size_period = c(100,50)


# regarding the use of miss_var_run, it might also be useful to summarise the frequency of the size of the runs
# so I want variables like:
# || run_size | n_missing | n_complete | prop_missing | prop_complete ||

# possible implementation for multiple spans

# pedestrian %>%
#   mutate(weekday = if_else(Day == "Saturday" | Day == "Sunday",
#                            true = "weekend",
#                            false = "weekday")) %>%
#   group_by(Sensor_Name,
#            weekday) %>%
#   # miss_ts_summary(time_index = Date_Time,
#   #                 variable = Hourly_Counts
#   # naniar:::add_period_counter(period_length = ) %>%
#   # dplyr::group_by(period_counter) %>%
#   dplyr::tally(is.na(Hourly_Counts))
# dplyr::rename(n_miss = n) %>%
#   dplyr::mutate(n_complete = length(dat_ts) - n_miss,
#                 prop_miss = n_miss / period,
#                 prop_complete = 1 - prop_miss)
#

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
