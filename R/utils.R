#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom rlang is_na
#' @export
rlang::is_na

#' @importFrom rlang are_na
#' @export
rlang::are_na

#' Group By Helper
#'
#' This is a wrapper to facilitate the `grouped_df` S3 method.
#'
#' @param data data.frame, which will be grouped
#' @param .fun a function to apply
#' @param ... additional arguments to be passed to map
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
group_by_fun <- function(data,.fun, ...){
  tidyr::nest(data) %>%
    dplyr::mutate(data = purrr::map(data, .fun, ...)) %>%
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

# are there any columns that contain a shadow column?
any_shadow <- function(x){
  any(grepl("_NA$",colnames(x)))
}

# # test if there are shadow columns?
test_if_any_shadow <- function(x){
  # test for dataframe
  test_if_dataframe(x)
  if (!any_shadow(x)) {
    stop("Input must contain a shadow column ending in _NA", call. = FALSE)
    }
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

# utility function to convert bare name to character
bare_to_chr <- function(...){
  ps <- rlang::exprs(...)

  exprs_text <- function(ps) {
    paste0(purrr::map_chr(ps, rlang::expr_text))
  }

  exprs_text(ps)

}

quo_to_shade <- function(...){

  quo_vars <- rlang::quos(...)

  shadow_chr <- bare_to_chr(!!!quo_vars) %>% paste0("_NA")

  shadow_vars <- rlang::syms(shadow_chr)

  return(shadow_vars)

}
