#' Create a shadow matrix
#'
#' \code{shadow_df} creates a shadow matrix/data frame of class \code{tbl_df} that denotes whether a given cell is missing or not - if a value is missing, it is denoted as TRUE.
#'
#' @param x a dataframe
#'
#' @examples
#'
#' shadow_df(airquality)
#'
#' @export

shadow_df <- function (x){

  # catch entries that are not dataframes
  stopifnot(is.data.frame(x))

  y <- if (length(x)) {
    dplyr::as_data_frame(
      lapply(x, "is.na")
    )
  }
  else dplyr::data_frame()
  y
}


#' Create factor levels of missingness for two variables
#'
#' \code{miss_cat} takes a data frame, df , and two variables as strings, var1 and var2, and converts them to a missing TRUE/FALSE matrix, where TRUE = missing. It then uses \code{interaction}, to create all the different levels of missingness - TRUE.TRUE, TRUE.FALSE, FALSE.TRUE, and FALSE.FALSE. It then uses the function \code{shadow_cat} to collapse across these and put them into the relevant categories
#'
#' @param df a dataframe
#'
#' @param var1 a dataframe
#'
#' @param var2 a dataframe
#'
#' @note Unsure if this function is actually needed anymore.

miss_cat <- function(df, var1, var2){
  df %>%
    # make the data into a true/false data frame
    shadow_df %>%
    # choose the variables of interest
    # dplyr::select_(quote(var1), quote(var2)) %>%
    dplyr::select(dplyr::one_of(var1, var2)) %>%
    # get all the combinations of the levels as factors
    interaction %>%
    # combine them into something sensible for our purposes
    shadow_cat
}

#' Categorise specific missings into missing  / not missing
#'
#' \code{shadow_cat} is a window function that rearranges factor levels from \code{interaction}.
#'
#' @param x dataframe coming from \code{interaction}

shadow_cat <- function(x){
  ifelse(x == "TRUE.FALSE" |
           x == "TRUE.TRUE" |
           x == "FALSE.TRUE",
         yes = "Missing",
         no = "Not Missing")
}
