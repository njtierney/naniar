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
#' @keywords internal
#' @noRd
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
    tidyr::unnest(cols = c(data))
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
#' @keywords internal
#' @noRd
test_if_null <- function(x){

  # test for null
  if (is.null(x)) {
    cli::cli_abort(
      c(
        "Input must not be NULL",
        "Input is {.cls {class(x)}}"
      )
    )
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
#' @keywords internal
#' @noRd
test_if_missing <- function(x, msg = NULL){

  # test for null
  if (missing(x)) {
    cli::cli_abort(
      c(
        "argument must be specified",
        "{msg}"
      )
    )
      }
  }

#' @keywords internal
#' @noRd
test_if_dots_missing <- function(..., msg = NULL){

  # test for null
  if (missing(...)) {
    cli::cli_abort(
      c(
        "argument must be specified",
        "{msg}"
      )
    )
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
#' @keywords internal
#' @noRd
test_if_dataframe <- function(x){
  # test for dataframe
  if (!inherits(x, "data.frame")) {
    cli::cli_abort(
      c(
        "Input must inherit from {.cls data.frame}",
        "We see class: {.cls {class(x)}}"
      )
    )
  }
}

test_if_any_shade <- function(x){
  # test for dataframe
  test_if_dataframe(x)
  if (!any_shade(x)) {
    cli::format_error(
      c(
        "Input must contain a shade column.",
        "See {.code ?shade}, {.code ?shade}, and {.code ?bind_shadow}"
      )
    )
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
#' # add_span_counter(pedestrian, span_size = 100)
#' }
add_span_counter <- function(data, span_size) {

  dplyr::mutate(data,
                span_counter = rep(x = 1:ceiling(nrow(data)),
                                   each = span_size,
                                   length.out = nrow(data)))
}

#' check the levels of many things
#'
#' this function is used internally to check what the levels are of the dataframe.
#'
#' @param x data.frame, usually
#'
#' @return a list containing the levels of everything
#' @keywords internal
#' @noRd
what_levels <- function(x) purrr::map(x, levels)

quo_to_shade <- function(...){

  # Use ensyms() rather than quos() because the latter allows
  # arbitrary expressions. These variables are forwarded to select(),
  # so potential expressions are `starts_with()`, `one_of()`, etc.
  # The naniar code generally assumes that only symbols are passed in
  # dots. `ensyms()` is a way of ensuring the input types.
  vars <- rlang::ensyms(...)

  # Adding `_NA` suffix to user symbols
  shadow_chr <- purrr::map(vars, as_string) %>% paste0("_NA")

  # Casting back to symbols
  shadow_vars <- rlang::syms(shadow_chr)

  return(shadow_vars)

}

class_glue <- function(x){
  class(x) %>% glue::glue_collapse(sep = ", ", last = ", or ")
}

diag_na <- function(size = 5){

  dna <- diag(x = NA,
              nrow = size,
              ncol = size)
  suppressMessages(
    tibble::as_tibble(dna,
                      .name_repair = "unique")) %>%
    set_names(paste0("x",seq_len(ncol(.))))
}

coerce_fct_na_explicit <- function(x){
  if (is.factor(x) & anyNA(x)) {
    forcats::fct_na_value_to_level(x, level = "NA")
  } else {
    x
  }
}

# any_shade <- function(x) any(grepl("^NA|^NA_", x))

any_row_shade <- function(x){
  apply(data.frame(x), MARGIN = 1, FUN = function(x) any(grepl("^NA|^NA_", x)))
}

vecIsFALSE <- Vectorize(isFALSE)

are_any_false <- function(x, ...) any(vecIsFALSE(x), ...)

check_btn_0_1 <- function(prop){
  if (prop < 0 || prop > 1) {
    cli::cli_abort(
      c(
        "{.var prop} must be between 0 and 1",
        "{.var prop} is {prop}"
      )
    )
  }
}

check_is_integer <- function(x){
  if (x < 0) {
    cli::cli_abort(
      c(
        "{.var x} must be greater than 0",
        "{.var x} is {.val {x}}"
      )
    )
  }
  vctrs::vec_cast(x, integer())
}

check_is_scalar <- function(x){
  if (length(x) != 1) {
    cli::cli_abort(
      c(
        "{.var x} must be length 1",
        "{.var x} is {x}, and {.var x} has length: {length(x)}"
      )
    )
  }
}
