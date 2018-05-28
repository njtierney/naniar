#' Search and present different kinds of missing values
#'
#' Searching for different kinds of missing values is really annoying. If
#'     you have values like -99 in your data, when they shouldn't be there,
#'     or they should be encoded as missing, it can be difficult to ascertain
#'     if they are there, and if so, where they are. `miss_scan_count` makes it
#'     easier for users to search for particular occurrences of these values
#'     across their variables.
#'
#' @param data data
#' @param search values to search for
#'
#' @return a dataframe of the occurences of the values you searched for
#'
#' @seealso [miss_case_pct]() [miss_case_prop]() [miss_prop_summary()] [miss_case_summary]() [miss_case_table]() [miss_summary]() [miss_var_pct]() [miss_var_prop]() [miss_var_run]() [miss_var_span]() [miss_var_summary]() [miss_var_table]()
#'
#' @export
#'
#' @examples
#'
#' dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#'                          1,   "A",   -100,
#'                          3,   "N/A", -99,
#'                          NA,  NA,    -98,
#'                          -99, "E",   -101,
#'                          -98, "F",   -1)
#'
#' miss_scan_count(dat_ms,-99)
#' miss_scan_count(dat_ms,c(-99,-98))
#' miss_scan_count(dat_ms,c("-99","-98","N/A"))
#' miss_scan_count(dat_ms,common_na_strings)
#'
miss_scan_count <- function(data,search){
  # if there is only one value to search
  if (length(search) == 1) {
    purrr::map_df(data,~length(grep(search,x = .))) %>%
      # return the dataframe with the columns "
      tidyr::gather(key = "Variable",
                    value = "n")
    # but if there are more than one, we need to combine the search terms
  } else if (length(search) > 1) {
    purrr::map_df(data,~length(grep(paste0(search, collapse ="|"),x = .))) %>%
      tidyr::gather(key = "Variable",
                    value = "n")
  }

}

