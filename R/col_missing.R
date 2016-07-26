#' col_missing
#'
#' Return a tidy dataframe that contains the number of missings in each column
#'
#' @param x a dataframe
#'
#' @return a dataframe
#' @export
#'
#' @examples
#'
#' col_missing(airquality)
#'
col_missing <- function(x) {
  x %>%
  is.na() %>%
  colSums() %>%
  tibble::as_data_frame() %>%
  dplyr::mutate(variable = names(x)) %>%
  dplyr::rename(n_missing = value) %>%
  dplyr::select(variable, n_missing)

} # end function
