#' Scan variables for particular possible missings
#'
#' @param data data.frame
#' @param scan list of values to scan
#'
#' @return summary data.frame of variables and how many scanned vars
#' @export
#'
#' @examples
#'
#' dat_ms <- tribble(~x,  ~y,    ~z,
#'                   3,   "N/A",  -99,
#'                   -99, "E",    97,
#'                   -98, "F",    97)
#'
#' miss_var_scan(dat_ms,
#'               list("N/A",
#'                    -99))
#'
#' miss_var_scan(dat_ms,
#'               list("N/A",
#'                    -99,
#'                    -98))
miss_var_scan <- function(data, scan){

  n_match <- function(x, scan){
    stopifnot(is.list(scan))
    map(scan, ~sum(grepl(., x)))
  }

  purrr::map(data,
             n_match,
             scan) %>%
    purrr::transpose() %>%
    purrr::map(transpose)  %>%
    dplyr::bind_cols() %>%
    dplyr::unnest() %>%
    rlang::set_names(scan) %>%
    dplyr::mutate(variables = names(data)) %>%
    janitor::adorn_totals(where = c("col")) %>%
    dplyr::select(variables,
                  Total,
           dplyr::everything()) %>%
    dplyr::rename(total = Total)

}
