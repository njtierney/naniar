#' miss_cat
#'
#' \code{miss_cat} creates factor levels of missingness for two variables
#'
#' @description miss_cat takes a data frame `df`, and two variables as strings, `var1` and `var2`, and converts them to a missing TRUE/FALSE matrix, where TRUE = missing. It then uses `interaction`, to create all the different levels of missingness - TRUE.TRUE, TRUE.FALSE, FALSE.TRUE, and FALSE.FALSE. It then uses the function `shadow_cat` to collapse across these and put them into the relevant categories
#'
#' @param df a dataframe
#'
#' @param var1 a dataframe
#'
#' @param var2 a dataframe
#'
#' @import dplyr
#'
#' @export
miss_cat <- function(df, var1, var2){
  df %>%
    # make the data into a true/false data frame
    shadow_df %>%
    # choose the variables of interest
    select(one_of(var1, var2)) %>%
    # get all the combinations of the levels as factors
    interaction %>%
    # combine them into something sensible for our purposes
    shadow_cat
}
