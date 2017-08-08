#' Give NAs a more meaningful label
#'
#' Returns a binary factor of !NA and NA, where !NA indicates a datum that is
#'   not missing, and NA indicates missingness. This function is what powers the
#'   factor levels in `as_shadow()`.
#'
#' @param x a vector
#'
#' @return a vector of factors containing the labels "!NA" for Not missing and
#'   "NA" for missing.
#'
#' @seealso as_shadow
#'
#' @examples
#' \dontrun{
#' label_shadow_matrix(airquality$Ozone)
#'}
label_shadow_matrix <- function(x) {
  if (length(x) == 0) {
    stop("Input is of length 0, please check your inputs.", call. = FALSE)
  } else{
    factor(is.na(x),
           levels = c(FALSE, TRUE),
           labels = c("!NA", "NA"))
  }
}

#' Label a missing from one column
#'
#' Label whether a value is missing in a row of one columns.
#'
#' @param x1 a variable of a dataframe
#'
#' @return a vector indicating whether any of these rows had missing values
#'
#' @note can we generalise label_miss to work for any number of variables?
#'
#' @export
#'
#' @examples
#'
#' label_miss_1d(airquality$Ozone)
#'
label_miss_1d <- function(x1){

  # Catch NULL entries
  test_if_null(x1)
  # find which are missing and which are not.
  temp <- data.frame(x1) %>% is.na %>% rowSums()
  ifelse(temp == 0, # 0 means not missing
         "Not Missing", # not missing
         "Missing") # missing

}

#' label_miss_2d
#'
#' Label whether a value is missing in either row of two columns.
#'
#' @param x1 a variable of a dataframe
#' @param x2 another variable of a dataframe
#'
#' @return a vector indicating whether any of these rows had missing values
#' @export
#'
#' @examples
#'
#' label_miss_2d(airquality$Ozone, airquality$Solar.R)
#'
label_miss_2d <- function(x1, x2){

  # Catch NULL entries
  if(is.null(x1) | is.null(x2)) stop("Input cannot be NULL", call. = FALSE)
  # find which are missing and which are not.
  temp <- data.frame(x1,x2) %>% is.na %>% rowSums()
  ifelse(temp == 0, # 0 means not missing
         "Not Missing", # not missing
         "Missing") # missing

}

