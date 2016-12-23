#' Add column containing number of missing data values
#'
#' It can be useful when doing data analysis to add the number of missing data points into your dataframe. add_n_miss adds a column named "n_miss", which contains the number of missing values in that row.
#'
#' @param data a dataframe
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' airquality %>% add_n_miss()
#'
#'
add_n_miss <- function(data){

  purrr::by_row(.d = data,
                ..f = function(x) n_miss(x),
                .collate = "row",
                .to = "n_miss")

  # old approach
  # # create a numeric vector of n_missing
  # col_n_miss <- data.frame(n_miss = apply(df,1,n_miss))
  #
  # dplyr::bind_cols(df,col_n_miss)

}

#' Add column containing proportion of missing data values
#'
#' It can be useful when doing data analysis to add the proportion of missing data values into your dataframe. add_prop_miss adds a column named "prop_miss", which contains the proportion of missing values in that row.
#'
#' @param data a dataframe
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#'
#' library(magrittr)
#' airquality %>% add_prop_miss()
#'
#' # this can be applied to model the proportion of missing data as in Tierney et al \url{bmjopen.bmj.com/content/5/6/e007450.full}
#' library(rpart)
#' library(rpart.plot)
#'
#' airquality %>%
#' add_prop_miss() %>%
#' rpart(prop_miss ~ ., data = .) %>%
#' prp(type = 4,
#'     extra = 101,
#'     prefix = "prop_miss = ")


add_prop_miss <- function(data){

  purrr::by_row(.d = data,
                ..f = function(x) (mean(is.na(x))),
                .collate = "row",
                .to = "prop_miss")

  # old approach
  # df %>%
  #   add_n_miss() %>%
  #   dplyr::mutate(pct_miss = n_miss/ncol(df)) %>%
  #   dplyr::select(-n_miss)

}

# purrr::by_row may be deprecated soon, keeping old methods below
# just as a fallback

