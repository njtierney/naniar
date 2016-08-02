#' label_missing_1d
#'
#' Label whether a value is missing in either row of two columns. This is a more appealing alternative to miss_cat, which seems a bit overcomplicated.
#'
#' @param x1 a variable of a dataframe
#'
#' @return a vector indicating whether any of these rows had missing values
#'
#' @note can we generalise label_missing to work for any number of variables?
#'
#' @export
#'
#' @examples
#'
#' label_missing_1d(airquality$Ozone)
#'
label_missing_1d <- function(x1){

  # find which are missing and which are not.
  temp <- data.frame(x1) %>% is.na %>% rowSums()
  ifelse(temp == 0, # 0 means not missing
         "Not Missing", # not missing
         "Missing") # missing

}
