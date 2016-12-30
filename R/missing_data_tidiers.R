#' Tidy methods for displaying missing data
#'
#' These methods prove tidy summaries of missing data information. The \code{percent_missing_df}, \code{percent_missing_var}, and \code{percent_missing_case} functions provide numeric summaries, for the percent of missing data for the data (\code{percent_missing_df}), the percent of variables that contain missing values (\code{percent_missing_var}), the percent of cases that contain mising values (\code{percent_missing_case}). \code{any_na} finds whether a vector contains a missing value. \code{table_missing_var} provides a data_frame of the number of variables with 0, 1, 2, up to n, missing values and the percent of that variable that is missing; \code{table_missing_case} provides a tidy table of the number of cases with 0, 1, 2, up to n, missing values, and the percent of the number of cases that are missing; \code{summary_missing_var} a data_frame of the percent of missing data in each variable; \code{summary_missing_case} provides the ratio of observations that have missings, and the number of cases that have at 0, 1, up to n, missing values' \code{summarise_missingness} returns a data_frame with the \code{percent_missing} as numeric, and table_missing_ and summary_missing_ and friends as lists, where each is a data_frame

#' Percentage of missing data in a dataframe
#'
#' Calculate the percent (%) of missing data in a dataframe.
#'
#' @param data a dataframe
#'
#' @return numeric the percent of missing data in a dataframe
#' @export
#'
#' @examples
#'
#' library(naniar)
#' percent_missing_df(airquality)
#'
percent_missing_df <- function(data){

  temp <- mean(is.na(data))
  temp * 100

}

#' Percentage of variables containing missings
#'
#' Calculate the percentage of variable that contain a single missing value.
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
#' percent_missing_var(airquality)
#'
percent_missing_var <- function(data){

  # which variables contain a missing value
  # find the proportion of variables that contain missing values
  temp <- mean(purrr::map_lgl(data, anyNA))

  # turn it into a percent
  temp * 100

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
#' percent_missing_case(airquality)
#'
percent_missing_case <- function(data){
data = airquality
  temp <- data %>%
    # which rows are complete?
    stats::complete.cases() %>%
    mean()

  (1 - temp) * 100

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

  tibble::tibble(df = percent_missing_df(data),
                 var = percent_missing_var(data),
                 case = percent_missing_case(data))

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
#' table_missing_case(airquality)
#'
#'
table_missing_case <- function(data){
  purrr::by_row(data,
                # how many are missing in each row?
                function(x) sum(is.na(x)),
                .collate = "row",
                .to = "n_missing_in_case") %>%
    dplyr::group_by(n_missing_in_case) %>%
    dplyr::tally() %>%
    dplyr::mutate(percent = (n / nrow(data) * 100)) %>%
    dplyr::rename(n_cases = n)

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
#' table_missing_var(airquality)
#'
#'
table_missing_var <- function(data){
  purrr::map_df(data, ~sum(is.na(.))) %>%
  tidyr::gather(key = "variable",
                value = "n_missing_in_var") %>%
    dplyr::group_by(n_missing_in_var) %>%
    dplyr::tally() %>%
    dplyr::rename(n_vars = n) %>%
    dplyr::mutate(percent = (n_vars / ncol(data) * 100))
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
#' summary_missing_var(airquality)
#'
  summary_missing_var <- function(data){
    purrr::dmap(data,
                # how many are missing in each variable?
                function(x) sum(is.na(x))) %>%
      tidyr::gather(key = "variable",
                    value = "n_missing") %>%
      dplyr::mutate(percent = (n_missing / nrow(data) * 100)) %>%
      dplyr::arrange(-n_missing)

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
#' summary_missing_case(airquality)
#'
summary_missing_case <- function(data){
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
#' s_miss <- summarise_missingness(airquality)
#' s_miss$percent_missing_df
#' s_miss$table_missing_case
#' # etc, etc, etc.
#'
summarise_missingness <- function(data){

  stopifnot(is.data.frame(data))

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

  }
