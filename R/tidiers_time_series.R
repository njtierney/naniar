#' Add a counter variable for the period of time
#'
#' This adds a period_counter variable to the dataframe. It is used internally
#' inside `narnia` to facilitate the counting of missing values over a given
#' time period.
#'
#' @param data data.frame
#' @param period_length integer
#'
#' @return data.frame with extra variable "period_counter".
#'
#' @examples
#' \dontrun{
#' library(imputeTS)
#' add_period_counter(as_tibble(tsNH4),
#'                    period_length = 3)
#' }

#'
add_period_counter <- function(data, period_length) {
  data %>%
    dplyr::mutate(period_counter = rep(x = 1:ceiling(nrow(data)),
                                       each = period_length,
                                       length.out = nrow(data)))
}

#' Summarise the number of missings in a given period for a ts object
#'
#' To summarise the missing values in a time series object it can be useful to
#'     calculate the number of missing values in a given time period.
#'     `miss_ts_summary` takes a `ts` object and a `period` argument and
#'      returns a dataframe containing the number of missing values within each
#'      period.
#'
#' @param dat_ts ts object
#' @param period integer describing the length of the period to be explored
#'
#' @return dataframe with variables `n_miss`, `n_complete`, `prop_miss`, and
#'     `prop_complete`, which describe the number, or proportion of missing or
#'     complete values within that given time period
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(imputeTS)
#' miss_ts_summary(data_ts = tsNH4,
#' period = 100)
#' }
#'
miss_ts_summary <- function(data_ts,
                            period){

  tibble::tibble(ts = data_ts) %>%
    add_period_counter(period_length = period) %>%
    dplyr::group_by(period_counter) %>%
    dplyr::tally(is.na(ts)) %>%
    dplyr::rename(n_miss = n) %>%
    dplyr::mutate(n_complete = length(data_ts) - n_miss,
                  prop_miss = n_miss / period,
                  prop_complete = 1 - prop_miss)

}

#' Plot the number of missings in a given period
#'
#' This is a replacement function to
#' imputeTS::plotNA.distributionBar(tsNH4, breaksize = 100), which shows the
#' number of missings in a given time period
#'
#' @param dat_ts ts object
#' @param period integer describing the length of the period to be explored
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' library(imputeTS)
#' gg_miss_ts(tsNH4, period = 100)
#' }
gg_miss_ts <- function(dat_ts,
                       period){

  miss_ts_summary(dat_ts,
                  period) %>%
    tidyr::gather(key = variable,
                  value = value,
                  prop_miss:prop_complete) %>%
    ggplot2::ggplot(ggplot2::aes(x = period_counter,
                                 y = value,
                                 fill = variable)) +
    ggplot2::geom_col(colour = "white") +
    ggplot2::scale_fill_manual(name = "",
                               values = c("grey80",
                                          "grey20"),
                               label = c("Present",
                                         "Missing")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Proportion of missing values",
                  subtitle = sprintf("Over a period of %s", period),
                  x = "Time Period",
                  y = "Proportion Missing")

}

#' Return the number of missing or complete values in a single run
#'
#' In time series it can be useful to determine the number of missing values
#'     that occur in a single run. This function `miss_ts_run` returns a
#'     dataframe with the column names "run_length" and "is_na", which describe
#'     the length of the run, and whether that run describes a missing value
#'
#' @param dat_ts a ts object
#'
#' @return dataframe with column names "run_length" and "is_na", which describe
#'     the length of the run, and whether that run describes a missing value.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(imputeTS)
#' library(ggplot2)
#' library(dplyr)
#'
#' # explore the number of missings in a given run
#' miss_ts_run(tsNH4) %>%
#' filter(is_na) %>%
#' count(run_length) %>%
#' ggplot(aes(x = run_length,
#'            y = n)) +
#'      geom_col()
#'
#' # look at the number of missing values and the run length of these.
#' miss_ts_run(tsNH4) %>%
#'   ggplot(aes(x = is_na,
#'              y = run_length)) +
#'      geom_boxplot()
#'
#'}
#'
miss_ts_run <- function(dat_ts){
  tibble::as_tibble(c(rle(is.na(dat_ts)))) %>%
    dplyr::rename(run_length = lengths,
                  is_na = values)
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
