#' row_missing
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
#' row_missing(airquality)
#'
row_missing <- function(x) {
  x %>%
    is.na() %>%
    rowSums() %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(row = 1:nrow(x)) %>%
    dplyr::rename(n_missing = value) %>%
    dplyr::select(row, n_missing) %>%
    dplyr::mutate(n_col = 1)

} # end function
