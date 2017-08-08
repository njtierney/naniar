#' Find the number of missing and complete values in a single run
#'
#' It us useful to find the number of missing values that occur in a single run.
#'    The function, `na_var_run()`, returns a dataframe with the column names
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
#' @export
#'
#' @examples
#'
#' na_var_run(pedestrian, hourly_counts)
#'
#' library(dplyr)
#'
#' # find the number of runs missing/complete for each month
#'
#' pedestrian %>%
#'   group_by(month) %>%
#'   na_var_run(hourly_counts)
#'
#' library(ggplot2)
#'
#' # explore the number of missings in a given run
#' na_var_run(pedestrian, hourly_counts) %>%
#'   filter(is_na == "missing") %>%
#'   count(run_length) %>%
#'   ggplot(aes(x = run_length,
#'              y = n)) +
#'       geom_col()
#'
#' # look at the number of missing values and the run length of these.
#' na_var_run(pedestrian, hourly_counts) %>%
#'   ggplot(aes(x = is_na,
#'              y = run_length)) +
#'       geom_boxplot()
#'
#'# using group_by
#'  pedestrian %>%
#'    group_by(month) %>%
#'    na_var_run(hourly_counts)
#'
#'
na_var_run <- function(data, var){

  test_if_null(data)

  test_if_missing(var)

  test_if_dataframe(data)

  UseMethod("na_var_run")

}

#' @export
na_var_run.default <- function(data, var){

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
na_var_run.grouped_df <- function(data,var){

  var <- rlang::enquo(var)

  tidyr::nest(data) %>%
    dplyr::mutate(data = purrr::map(data,
                                    var = !!var,
                                    .f = na_var_run)) %>%
    tidyr::unnest()

}
