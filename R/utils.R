#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Group By Helper
#'
#' This is a wrapper to facilitate the `grouped_df` S3 method.
#'
#' @param data data.frame, which will be grouped
#' @param .fun a function to apply
#'
#' @return a dataframe with the function applied to each group
#'
#' @examples
#'
#' \dontrun{
#' miss_case_table.grouped_df <- function(data){
#' group_by_fun(data,.fun = miss_case_table)
#' }
#' airquality %>%
#' group_by(Month) %>%
#' miss_case_table()
#' }
#'
group_by_fun <- function(data,.fun){
  tidyr::nest(data) %>%
    dplyr::mutate(data = purrr::map(data, .fun)) %>%
    tidyr::unnest()
}


#' Test if the input is NULL
#'
#' @param x object
#'
#' @return an error if input (x) is NULL
#'
#' @examples
#' \dontrun{
#' # success
#' test_if_null(airquality)
#' #fail
#' my_test <- NULL
#' test_if_null(my_test)
#' }
test_if_null <- function(x){

  # test for null
  if (is.null(x)) {
    stop("Input must not be NULL", call. = FALSE)
    }
}

#' Test if the input is Missing
#'
#' @param x object
#'
#' @return an error if input (x) is not specified
#'
#' @examples
#' \dontrun{
#' # success
#' my_test <- x
#' test_if_null(my_test)
#' #fail
#' test_if_missing()
#' }
test_if_missing <- function(x){

  # test for null
  if (missing(x)) {
    stop("argument must be specified", call. = FALSE)
    }
}

#' Test if input is a data.frame
#'
#' @param x object
#'
#' @return an error if input (x) is a data.frame
#'
#' @examples
#' \dontrun{
#' # success
#' test_if_dataframe(airquality)
#' #fail
#' my_test <- matrix(10)
#' test_if_dataframe(my_test)
#' }
#'
test_if_dataframe <- function(x){
  # test for dataframe
  if (!inherits(x, "data.frame")) {
    stop("Input must inherit from data.frame", call. = FALSE)
    }
}

#' Which rows and cols contain missings?
#'
#' Internal function that is short for `which(is.na(x))`. Creates integer
#'   locations of missing values in a dataframe. May be used in future `impl_df`
#'   class.
#'
#' @param x a dataframe
#'
#' @return integers that describe the location of missing values
#'
#' @seealso which_na
#'
#' @examples
#'
#' naniar:::where_na(airquality)
#'
where_na <- function(x){
  which(is.na(x), arr.ind = TRUE)
}

#' Which elements contain missings?
#'
#' Internal function that creates a matrix containing the location of missing
#'   values in a dataframe. This may be used in the future `impl_df` class.
#'
#' @param x a dataframe
#'
#' @return a matrix with columns "row" and "col", which refer to the row and
#'     column that identify the position of a missing value in a dataframe
#'
#' @seealso where_na
#'
#' @examples
#'
#' naniar:::which_na(airquality)
#'
which_na <- function(x){
  which(is.na(x))
}

#' Helper function to determine whether there are any missings
#'
#' @param x a vector
#'
#' @return logical vector TRUE = missing FALSE = complete
#'
any_row_miss <- function(x){
  apply(data.frame(x), MARGIN = 1, FUN = function(x) anyNA(x))
}

#' Helper function to determine whether all rows are missing
#'
#' @param x a vector
#'
#' @return logical vector
all_row_miss <- function(x){
  apply(data.frame(x), MARGIN = 1, FUN = function(x) all(is.na(x)))
}

#' Helper function to determine whether all rows are complete
#'
#' @param x a vector
#'
#' @return logical vector
all_row_complete <- function(x){
  apply(data.frame(x), MARGIN = 1, FUN = function(x) all(!is.na(x)))
}

#' Add a counter variable for a span of dataframe
#'
#' Adds a variable, `span_counter` to a dataframe. Used internally to facilitate
#' counting of missing values over a given span.
#'
#' @param data data.frame
#' @param span_size integer
#'
#' @return data.frame with extra variable "span_counter".
#'
#' @examples
#' \dontrun{
#' add_span_counter(pedestrian, span_size = 100)
#' }
add_span_counter <- function(data, span_size) {

  dplyr::mutate(data,
                span_counter = rep(x = 1:ceiling(nrow(data)),
                                   each = span_size,
                                   length.out = nrow(data)))
}
