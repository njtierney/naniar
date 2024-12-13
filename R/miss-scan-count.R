#' Search and present different kinds of missing values
#'
#' Searching for different kinds of missing values is really annoying. If
#'     you have values like -99 in your data, when they shouldn't be there,
#'     or they should be encoded as missing, it can be difficult to ascertain
#'     if they are there, and if so, where they are. `miss_scan_count` makes it
#'     easier for users to search for particular occurrences of these values
#'     across their variables. Note that the searches are done with regular
#'     expressions, which are special ways of searching for text. See the
#'     example below to see how to look for characters like `?`.
#'
#' @param data data
#' @param search values to search for
#'
#' @return a dataframe of the occurrences of the values you searched for
#'
#' @seealso  [pct_miss_case()] [prop_miss_case()] [pct_miss_var()] [prop_miss_var()] [pct_complete_case()] [prop_complete_case()] [pct_complete_var()] [prop_complete_var()] [miss_prop_summary()] [miss_case_summary()] [miss_case_table()] [miss_summary()] [miss_var_prop()] [miss_var_run()] [miss_var_span()] [miss_var_summary()] [miss_var_table()]
#'
#' @export
#'
#' @examples
#'
#' dat_ms <- tibble::tribble(~x,  ~y,    ~z,  ~specials,
#'                          1,   "A",   -100, "?",
#'                          3,   "N/A", -99,  "!",
#'                          NA,  NA,    -98,  ".",
#'                          -99, "E",   -101, "*",
#'                          -98, "F",   -1,  "-")
#'
#' miss_scan_count(dat_ms,-99)
#' miss_scan_count(dat_ms,c(-99,-98))
#' miss_scan_count(dat_ms,c("-99","-98","N/A"))
#' miss_scan_count(dat_ms, "\\?")
#' miss_scan_count(dat_ms, "\\!")
#' miss_scan_count(dat_ms, "\\.")
#' miss_scan_count(dat_ms, "\\*")
#' miss_scan_count(dat_ms, "-")
#' miss_scan_count(dat_ms,common_na_strings)
#'
#'
miss_scan_count <- function(data,search){

  pattern <- paste0(search, collapse = "|")

  res <- data %>%
    # if the value is in the search terms, return TRUE
    dplyr::mutate(dplyr::across(dplyr::everything(), ~grepl(pattern, .x, perl = TRUE))) %>%
    # sum the number of times the value is found
    dplyr::summarise(dplyr::across(dplyr::everything(), sum)) %>%
    # present the data in a long format
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Variable", values_to = "n") %>%
    # calculate the percentage
    dplyr::mutate(pct = (n/nrow(data))*100) %>%
    # order the data in descending order
    dplyr::arrange(-n)

  return(res)
}

