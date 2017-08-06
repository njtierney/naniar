#' Give NAs a more meaningful label
#'
#' Returns a binary factor of !NA and NA, where !NA indicates a datum that is not
#'   missing, and NA indicates missingness.
#'
#' @param x a vector
#'
#' @return a vector
#' @export
#'
#' @seealso as_shadow
#'
#' @examples
#'
#' label_na(airquality$Ozone)
#'
label_na <- function(x) {
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
  if(is.null(x1)) stop("Input cannot be NULL", call. = FALSE)
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

#' Is there a missing value in the row of a dataframe?
#'
#' Creates a character vector describing whether there are missing or not missing
#'   values
#'
#' @param data a dataframe or set of vectors of the same length
#'
#' @return character vector of "Missing" and "Not Missing".
#'
#' @export
#'
#' @note should be an add_missing_labels function so you can do `data %>% add_missing_labels()`
#'
#' @examples
#'
#' label_missings(airquality)
#'
#' library(dplyr)
#'
#' airquality %>% mutate(is_missing = label_missings(airquality))
#'
label_missings <- function(data){

  # Catch NULL entries
  if(is.null(data)) stop("Input cannot be NULL", call. = FALSE)
  # find which are missing and which are not.

  any_row_na <- function(x){
    apply(data.frame(x), MARGIN = 1, FUN = function(x) anyNA(x))
  }

  temp <- any_row_na(data)
  dplyr::if_else(condition = temp == TRUE, # TRUE means missing
                 true = "Missing",
                 false = "Not Missing")

}

# there should be a missing labeller...

# label_na(data,
#          missing_level = ,
#          missing_label = ,
#          complete_level = ,
#          complete_label =
# )
#
#
#
# forcats::fct_explicit_na()
