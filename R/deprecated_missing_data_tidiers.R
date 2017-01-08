#' Percentage of missing data in a dataframe
#'
#'
#' @rdname miss_pct_df
#' @export
#'
#'
percent_missing_df <- function(data){

  # test for null input
  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
    .Deprecated("miss_df_pct")
    temp <- mean(is.na(data))
    temp * 100
  } else stop("Input must inherit from data.frame", call. = FALSE)

}

#' Percentage of variables containing missings
#'
#' @export
#' @rdname miss_var_pct
#'
#'
percent_missing_var <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
    .Deprecated("miss_var_pct")
    # which variables contain a missing value
    # find the proportion of variables that contain missing values
    temp <- mean(purrr::map_lgl(data, anyNA))

    # turn it into a percent
    temp * 100
  } else stop("Input must inherit from data.frame", call. = FALSE)

  # previous code
  # varmissingpct <- mean(sapply(dat,function(avec){any(is.na(avec))}))*100

} # end function

#' Percentage of cases that contain a missing values.
#'
#' @export
#' @rdname miss_case_pct
#'
percent_missing_case <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
    .Deprecated("miss_case_pct")
    temp <- data %>%
      # which rows are complete?
      stats::complete.cases() %>%
      mean()

    (1 - temp) * 100
  } else stop("Input must inherit from data.frame", call. = FALSE)
  # previous
  # casemissingpct <- 1-mean(complete.cases(dat))*100

}

#' Tabulate missings in cases.
#'
#' @export
#'
#' @rdname miss_case_table
#'
table_missing_case <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
    .Deprecated("miss_case_table")
    purrr::by_row(data,
                  # how many are missing in each row?
                  function(x) sum(is.na(x)),
                  .collate = "row",
                  .to = "n_missing_in_case") %>%
      dplyr::group_by(n_missing_in_case) %>%
      dplyr::tally() %>%
      dplyr::mutate(percent = (n / nrow(data) * 100)) %>%
      dplyr::rename(n_cases = n)
  } else stop("Input must inherit from data.frame", call. = FALSE)

  # previous
  # No_of_Case_missing <- table(apply(dat,
  #                                   1,
  #                                   function(avec){sum(is.na(avec))}))

}

#' Tabulate the missings in the variables
#'
#' @export
#'
#' @rdname miss_var_table
#'
table_missing_var <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
    .Deprecated("miss_var_table")
    purrr::map_df(data, ~sum(is.na(.))) %>%
      tidyr::gather(key = "variable",
                    value = "n_missing_in_var") %>%
      dplyr::group_by(n_missing_in_var) %>%
      dplyr::tally() %>%
      dplyr::rename(n_vars = n) %>%
      dplyr::mutate(percent = (n_vars / ncol(data) * 100))
  } else stop("Input must inherit from data.frame", call. = FALSE)
  # previous
  # No_of_Case_missing <- table(apply(dat,
  #                                   1,
  #                                   function(avec){sum(is.na(avec))}))
  # tidyverse
  # In each row, how many are missing?

}

#' Summarise the missingness in each variable
#'
#' @export
#'
#' @rdname miss_var_summary
#'
summary_missing_var <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
    .Deprecated("miss_var_summary")
    purrr::dmap(data,
                # how many are missing in each variable?
                function(x) sum(is.na(x))) %>%
      tidyr::gather(key = "variable",
                    value = "n_missing") %>%
      dplyr::mutate(percent = (n_missing / nrow(data) * 100)) %>%
      dplyr::arrange(-n_missing)

  } else stop("Input must inherit from data.frame", call. = FALSE)
}

#' Summarise the missingness in each case
#'
#' @export
#'
#' @rdname miss_case_summary
#'
summary_missing_case <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
    .Deprecated("miss_case_summary")
    purrr::by_row(.d = data,
                  ..f = function(x) (mean(is.na(x)) * 100),
                  .collate = "row",
                  .to = "percent") %>%
      purrr::by_row(.d = .,
                    ..f = function(x) (sum(is.na(x))),
                    .collate = "row",
                    .to = "n_missing") %>%
      dplyr::mutate(case = 1:nrow(data)) %>%
      dplyr::select(case,
                    n_missing,
                    percent)
  } else stop("Input must inherit from data.frame", call. = FALSE)
}

#' Collate summary measures from naniar into one tibble
#'
#' @export
#'
#' @rdname miss_summary
#'
summarise_missingness <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
    .Deprecated("miss_summary")
    return(
      tibble::data_frame(
        percent_missing_df = percent_missing_df(data),
        percent_missing_var = percent_missing_var(data),
        percent_missing_case = percent_missing_case(data),
        table_missing_case = list(table_missing_case(data)),
        table_missing_var = list(table_missing_var(data)),
        summary_missing_var = list(summary_missing_var(data)),
        summary_missing_case = list(summary_missing_case(data))
      )
    )
  } else stop("Input must inherit from data.frame", call. = FALSE)
}
