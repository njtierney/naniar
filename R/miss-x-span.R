#' Summarise the number of missings for a given repeating span on a variable
#'
#' To summarise the missing values in a time series object it can be useful to
#'     calculate the number of missing values in a given time period.
#'     `miss_var_span` takes a data.frame object, a variable, and a `span_every`
#'     argument and returns a dataframe containing the number of missing values
#'     within each span. When the number of observations isn't a perfect
#'     multiple of the span length, the final span is whatever the last
#'     remainder is. For example, the `pedestrian` dataset has 37,700 rows. If
#'     the span is set to 4000, then there will be 1700 rows remaining. This can
#'     be provided using modulo (`%%`): `nrow(data) %% 4000`. This remainder
#'     number is provided in `n_in_span`.
#'
#' @param data data.frame
#' @param var bare unquoted variable name of interest.
#' @param span_every integer describing the length of the span to be explored

#' @return dataframe with variables `n_miss`, `n_complete`, `prop_miss`, and
#'     `prop_complete`, which describe the number, or proportion of missing or
#'     complete values within that given time span. The final variable,
#'     `n_in_span` states how many observations are in the span.
#'
#' @export
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
#'
#' @examples
#'
#'miss_var_span(data = pedestrian,
#'              var = hourly_counts,
#'              span_every = 168)
#'
#' \dontrun{
#'  library(dplyr)
#'  pedestrian %>%
#'    group_by(month) %>%
#'      miss_var_span(var = hourly_counts,
#'                    span_every = 168)
#' }
miss_var_span <- function(data, var, span_every) {
  test_if_null(data)

  test_if_dataframe(data)

  test_if_missing(var)

  test_if_missing(span_every)

  UseMethod("miss_var_span")
}

#' @export
miss_var_span.default <- function(data, var, span_every) {
  dat_ts_summary <- dplyr::select(data, {{ var }})

  dat_ts_summary_span <- dat_ts_summary %>%
    # need to make add_span_counter respect grouping structure, somehow
    add_span_counter(span_size = span_every)

  dat_ts_summary_span_count <- dat_ts_summary_span %>%
    dplyr::count(span_counter, name = "n_in_span")

  dat_ts_summary_span %>%
    dplyr::group_by(span_counter) %>%
    dplyr::tally(is.na({{ var }})) %>%
    dplyr::left_join(dat_ts_summary_span_count, by = "span_counter") %>%
    dplyr::rename(n_miss = n) %>%
    dplyr::mutate(
      n_complete = n_in_span - n_miss,
      prop_miss = n_miss / n_in_span,
      prop_complete = 1 - prop_miss
    ) %>%
    dplyr::relocate(n_in_span, .after = prop_complete)
}

#' @export
miss_var_span.grouped_df <- function(data, var, span_every) {
  var <- rlang::enquo(var)

  tidyr::nest(data) %>%
    dplyr::mutate(
      data = purrr::map(
        .x = data,
        .f = miss_var_span,
        var = !!var,
        span_every = span_every
      )
    ) %>%
    tidyr::unnest(cols = c(data))
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
