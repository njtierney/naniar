# Tidy methods for displaying missing data

#' Percentage of missing data in a dataframe
#'
#' Calculate the percent of missing data in a dataframe.
#'
#' @param data a dataframe
#'
#' @return numeric the percent of missing data in a dataframe
#' @export
#'
#' @examples
#'
#' library(naniar)
#' miss_df_pct(airquality)
#'
miss_df_pct <- function(data){

  # test for null input
  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
    temp <- mean(is.na(data))
    temp * 100
    } else stop("Input must inherit from data.frame", call. = FALSE)

}

#' Percentage of variables containing missings
#'
#' Calculate the percentage of variables that contain a single missing value.
#'
#' @param data a dataframe
#'
#' @return numeric the percent of variables that contain missing data
#'
#' @export
#'
#' @examples
#'
#' library(naniar)
#'
#' miss_var_pct(airquality)
#'
miss_var_pct <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
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
#' Calculate the percentage of cases (rows) that contain a missing value.
#'
#' @param data a dataframe
#'
#' @return numeric the percentage of cases that contain a missing value
#' @export
#'
#' @examples
#'
#' miss_case_pct(airquality)
#'
miss_case_pct <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
  temp <- data %>%
    # which rows are complete?
    stats::complete.cases() %>%
    mean()

  (1 - temp) * 100
  } else stop("Input must inherit from data.frame", call. = FALSE)
  # previous
  # casemissingpct <- 1-mean(complete.cases(dat))*100

}

#' Proporitons of missings in data, variables, and cases.
#'
#' Return missing data info about the dataframe, the variables, and the cases. Specifically, returning how many elements in a dataframe contain a missing value, how many elements in a variable contain a missing value, and how many elements in a case contain a missing.
#'
#' @param data a dataframe
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' prop_na(airquality)
#'
prop_na <- function(data){


  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
  tibble::tibble(df = miss_df_pct(data),
                 var = miss_var_pct(data),
                 case = miss_case_pct(data))
  } else stop("Input must inherit from data.frame", call. = FALSE)

}


#' Tabulate missings in cases.
#'
#' Provide a tidy table of the number of cases with 0, 1, 2, up to n, missing values and the proportion of the number of cases those cases make up
#'
#' @param data a dataframe
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' miss_case_table(airquality)
#'
#'
miss_case_table <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
  purrrlyr::by_row(data,
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
#' Provide a tidy table of the number of variables with 0, 1, 2, up to n, missing values and the proportion of the number of variablers those variables make up
#'
#' @param data a dataframe
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' miss_var_table(airquality)
#'
#'
miss_var_table <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){
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
#' Provide a data_frame containing the variable names, the number of missing values, in each variable, and the percent of missing values in each variable.
#'
#' @param data a dataframe
#'
#' @return a data_frame of the percent of missing data in each variable
#' @export
#'
#' @examples
#'
#' miss_var_summary(airquality)
#'
miss_var_summary <- function(data){

    if(is.null(data)){
      stop("Input must not be NULL", call. = FALSE)
      # test for dataframe
    } else if(inherits(data, "data.frame")){
    purrr::map_df(data,
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
#' Provide a data_frame containing each case (row), the number of missing values in each case,  and the percent of missing values in each case.
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

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){

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
                  percent)
  } else stop("Input must inherit from data.frame", call. = FALSE)
}

#' Collate summary measures from naniar into one tibble
#'
#' \code{summarise_missingness} performs all of the missing data helper summaries and puts them into a list. Perhaps in the future this can all be some sort of nested dataframe?
#'
#' @param data a dataframe
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' s_miss <- miss_summary(airquality)
#' s_miss$percent_missing_df
#' s_miss$table_missing_case
#' # etc, etc, etc.
#'
miss_summary <- function(data){

  if(is.null(data)){
    stop("Input must not be NULL", call. = FALSE)
    # test for dataframe
  } else if(inherits(data, "data.frame")){

  return(
    tibble::data_frame(
        miss_df_pct = miss_df_pct(data),
        miss_var_pct = miss_var_pct(data),
        miss_case_pct = miss_case_pct(data),
        miss_case_table = list(miss_case_table(data)),
        miss_var_table = list(miss_var_table(data)),
        miss_var_summary = list(miss_var_summary(data)),
        miss_case_summary = list(miss_case_summary(data))
      )
    )
  } else stop("Input must inherit from data.frame", call. = FALSE)
  }
