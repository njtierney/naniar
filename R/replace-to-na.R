#' Replace to NA
#'
#' Replace one or more character string(s) with a NA
#'
#' @param x the dataframe or vector containing the strings to replace
#' @param ... the character strings
#' @importFrom stringr str_replace_all
#'
#' @return a dataframe or vector
#' @export
#'
#' @examples
#' replace_to_na(breizh, "not_available", "empty")
#' replace_to_na(breizh$Taille, "not_available", "empty")
#'

replace_to_na <- function(x, ...) UseMethod("replace_to_na")

# NULL -------------------------------------------------------------------------

#' @export
replace_to_na.NULL <- function(x, ...) NULL

# default ----------------------------------------------------------------------

#' @export
replace_to_na.default <- function(x, ...){
  warning(
    "shadow_shift does not know how to deal with data of class ",
    class(x)
  )

}

# numeric ----------------------------------------------------------------------

#' @export
replace_to_na.numeric <- function(x, ...){
  extra <- c(...) %>% paste(collapse = "|")
  stringr::str_replace_all(x, pattern = extra, replacement = NA_character_)
}

# factor ----------------------------------------------------------------------

#' @export
replace_to_na.factor <- function(x, ...){
  extra <- c(...) %>% paste(collapse = "|")
  stringr::str_replace_all(x, pattern = extra, replacement = NA_character_)
}

# data.frame ----------------------------------------------------------------------

#' @export
replace_to_na.data.frame <- function(x, ...){
  extra <- c(...) %>% paste(collapse = "|")
  purrr::map_df(x, stringr::str_replace_all, pattern = extra, replacement = NA_character_)
}
