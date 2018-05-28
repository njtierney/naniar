#' Common string values for NA
#'
#' This vector contains common values of NA (missing), which is aimed to
#'   be used inside naniar functions [miss_scan_count()] and
#'
#' @name common_na_strings
#' @docType data
#' @examples
#'
#' dat_ms <- tibble::tribble(~x,  ~y,    ~z,
#'                           1,   "A",   -100,
#'                           3,   "N/A", -99,
#'                           NA,  NA,    -98,
#'                           -99, "E",   -101,
#'                           -98, "F",   -1)
#'
#' miss_scan_count(dat_ms, -99)
#' miss_scan_count(dat_ms, c(-99,-98))
#' miss_scan_count(dat_ms, c("-99","-98","N/A"))
#' miss_scan_count(dat_ms, common_na_strings)
"common_na_strings"
