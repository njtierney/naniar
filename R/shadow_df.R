#' shadow_df
#'
#' @description \code{shadow_df} creates a shadow matrix/data frame of class 'tbl_df' that denotes whether a given cell is missing or not - if a value is missing, it is denoted as TRUE
#'
#' @param x a dataframe
#'
#' @import dplyr
#'
#' @export

shadow_df <- function(x){
  x %>%
    is.na.data.frame %>%
    as.data.frame %>%
    as_data_frame
}
