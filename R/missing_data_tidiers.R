#' Tidy methods for displaying missing data
#'
#' These methods prove tidy output of missing data information, with functions \code{percent_missing_*} containing the percent of missing data for the data (\code{percent_missing_df}), the percent of variables that contain missing values (\code{percent_missing_var}), the percent of cases that contain mising values (\code{percent_missing_case}). \code{any_na} finds whether a vector contains a missing value. \code{tally_var_missing} provides a data_frame of the number of variables with 0, 1, 2, up to n, missing values and the percent of that variable that is missing; \code{tally_case_missing} provides a tidy table of the number of cases with 0, 1, 2, up to n, missing values, and the percent of the number of cases that are missing; \code{summary_missing_var} a data_frame of the percent of missing data in each variable; \code{summary_missing_case} provides the ratio of observations that have missings, and the number of cases that have at 0, 1, up to n, missing values' \code{summarise_missingness}

#' percent_missing_df
#'
#' calculate percent of missing data in a dataframe.
#'
#' @param dat a dataframe
#'
#' @return numeric the percent of missing data in a dataframe
#' @export
#'
#' @examples
#'
#' library(ggmissing)
#' percent_missing_df(airquality)
#'
percent_missing_df <- function(dat){

  dat %>% is.na() %>% mean()

  # previous code
  # totalmissingpct <- mean(is.na(dat))

}

#' any_na
#'
#' find whether a vector contains a missing value. Used internally.
#'
#' @param x a vector
#'
#' @return Logical TRUE / FALSE
#' @export
#'
#' @examples
#'
#' library(ggmissing)
#' any_na(airqality$Solar.R)
#' any_na(airqality$Wind)
#'
any_na <- function(x){
  # does a vector contain a missing value?
  x %>% is.na %>% any
}

#' percent_missing_var
#'
#' @param dat a dataframe
#'
#' @return numeric the percent of variables that contain missing data
#'
#' @export
#'
#' @examples
#'
#' library(ggmissing)
#'
#' percent_missing_var(airquality)
#'
percent_missing_var <- function(dat){

  # which variables contain a missing value
  # find the proportion of variables that contain missing values
  temp <- purrr::map_lgl(dat, any_na) %>% mean

  # turn it into a percent
  temp * 100

  # previous code
  # varmissingpct <- mean(sapply(dat,function(avec){any(is.na(avec))}))*100

} # end function

#' percent_missing_case
#'
#' calculate percent of missing data in each case (row)
#'
#' @param dat a dataframe
#'
#' @return numeric the percentage of cases that contain a missing value
#' @export
#'
#' @examples
#'
#' library(ggmissing)
#' percent_missing_case(airquality)
#'
percent_missing_case <- function(dat){

  temp <-
    dat %>%
    # which rows are complete?
    complete.cases() %>%
    mean()

  (1 - temp) * 100

  # un-tidyverse
  # casemissingpct <- 1-mean(complete.cases(dat))*100

}

#' tally_case_missing
#'
#' provide a tidy table of the number of cases with 0, 1, 2, up to n, missing values and the proportion of the number of cases those cases make up
#'
#' @param dat a dataframe
#'
#' @return a dataframe of the
#' @export
#'
#' @examples
#'
#' tally_case_missing(airquality)
#'
#'
tally_case_missing <- function(dat){
  purrr::by_row(dat,
                # how many are missing in each row?
                function(x) sum(is.na(x)),
                .collate = "rows",
                .to = "n_missing_in_case") %>%
    dplyr::group_by(n_missing_in_case) %>%
    dplyr::tally() %>%
    dplyr::mutate(percent = (n / nrow(dat) * 100))
# un-tidyverse
# No_of_Case_missing <- table(apply(dat,
#                                   1,
#                                   function(avec){sum(is.na(avec))}))
# tidyverse
# In each row, how many are missing?

}

#' tally_var_missing
#'
#' provide a data_frame of the number of variables with 0, 1, 2, up to n, missing values and the proportion of the number of variables those variables make up
#'
#' @param dat a dataframe
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' tally_var_missing(airquality)
#'
tally_var_missing <- function(dat){
  purrr::dmap(dat,
              # how many are missing in each variable?
              function(x) sum(is.na(x))) %>%
    tidyr::gather(key = "variables",
                  value = "n_missing") %>%
    dplyr::mutate(percent = (n_missing / nrow(dat) * 100))
} # end function

#' summary_missing_var
#'
#' @param dat a dataframe
#'
#' @return a data_frame of the percent of missing data in each variable
#' @export
#'
#' @examples
#'
#' summary_missing_var(airquality)
#'
  summary_missing_var <- function(dat){
    purrr::dmap(dat,
                function(x) mean(is.na(x))) %>%
      tidyr::gather(key = "variable",
                    value = "percent_missing") %>%
      dplyr::mutate(percent_missing = percent_missing*100) %>%
      dplyr::arrange(-percent_missing)
  }

#' summary_missing_case
#'
#' @param dat a dataframe
#'
#' @return a data_frame of the percent of missing data in each case
#' @export
#'
#' @examples
#'
#' summary_missing_case(airquality)
#'
summary_missing_case <- function(dat){
  purrr::by_row(.d = dat,
                ..f = function(x) (mean(is.na(x)) * 100),
                .collate = "rows",
                .to = "percent_missing") %>%
    dplyr::mutate(case = 1:nrow(dat)) %>%
    dplyr::select(case,
                  percent_missing)
}

#' summarise_missingness
#'
#' summarise_missingness performs all of the missing data helper summaries and puts them into a list. Perhaps in the future this can all be some sort of nested dataframe?
#'
#' @param dat a dataframe
#'
#' @return a list of
#' @export
#'
#' @examples
#'
#' summarise_missingness(airquality)
#'
summarise_missingness <- function(dat){

  stopifnot(is.data.frame(dat))

  return(
    list(
        percent_missing_df = percent_missing_df(dat),
        percent_missing_var = percent_missing_var(dat),
        percent_missing_case = percent_missing_case(dat),
        tally_var_missing = tally_var_missing(dat),
        tally_case_missing = tally_case_missing(dat),
        summary_missing_var = summary_missing_var(dat),
        summary_missing_case = summary_missing_case(dat)
      )
    )

  }
