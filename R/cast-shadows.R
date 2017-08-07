#' Add a shadow column to a dataset
#'
#' Casting a shadow shifted column performs the equivalent pattern to
#'   "data %>% select(var) %>% shadow_shift()". This is a convenience function
#'   that makes it easy to perform certain visualisations, in line with the
#'   principle that the user should have a way to flexibly return dataformats
#'   containing information about the missing data. It also respects the dplyr
#'   verbs `starts_with`, `contains`, `ends_with`, etc.
#'
#' @param data data.frame
#' @param ... One or more unquoted expressions separated by commas. These
#'   respect the dplyr verbs `starts_with`, `contains`, `ends_with`, etc.
#'
#' @return data with the added variable shifted and the suffix `_NA`
#'
#' @export
#'
#' @examples
#'
#' airquality %>% cast_shadow(Ozone)
#' airquality %>% cast_shadow(Ozone, Solar.R)
#'
cast_shadow <- function(data, ...){

  if (missing(...)) {
    stop("please include variables to be selected after the data")
  } else {

    quo_vars <- rlang::quos(...)

    shadow_vars <- dplyr::select(data, !!!quo_vars) %>% as_shadow()

    my_data <- dplyr::select(data, !!!quo_vars)

    tibble::as_tibble(dplyr::bind_cols(my_data, shadow_vars))

  } # close else loop

}

#' Add a shadow and a shadow_shift column to a dataset
#'
#' Shift the values and add a shadow column.  It also respects the dplyr
#'   verbs `starts_with`, `contains`, `ends_with`, etc.
#' @param data data.frame
#' @param ... One or more unquoted expressions separated by commas. These also
#'   respect the dplyr verbs "starts_with", "contains", "ends_with", etc.
#'
#' @return data.frame with the shadow and shadow_shift vars
#'
#' @export
#'
#' @examples
#'
#' airquality %>% cast_shadow_shift(Ozone)
#' airquality %>% cast_shadow_shift(Ozone,Temp)
#'
#' airquality %>% cast_shadow_shift(dplyr::contains("o"))
#'
cast_shadow_shift <- function(data, ...){

  quo_vars <- rlang::quos(...)

  shadow_vars <- dplyr::select(data, !!!quo_vars) %>% cast_shadow(...)

  # shift those values selected
  add_shadow_shift(shadow_vars, ...)

}

#' Add a shadow column and a shadow shifted column to a dataset
#'
#' Shift the values, add shadow, add missing label
#'
#' @param data data.frame
#' @param ... One or more unquoted expressions separated by commas. These also
#'   respect the dplyr verbs "starts_with", "contains", "ends_with", etc.
#'
#' @return data.frame with the shadow and shadow_shift vars, and missing labels
#' @export
#'
#' @examples
#'
#' airquality %>% cast_shadow_shift_label(Ozone)
#' airquality %>% cast_shadow_shift_label(Ozone, Solar.R)
#'
cast_shadow_shift_label <- function(data, ...){

  if (missing(...)) {
    stop("please include variables to be selected after the data")
  } else {

    quo_vars <- rlang::quos(...)

    shadow_vars <- dplyr::select(data, !!!quo_vars) %>% cast_shadow(...)

    # shift those values selected
    add_shadow_shift(shadow_vars, ...) %>% add_label_missings()

  } # close else loop

}


# perhaps what I need are functions like:
# add_any_miss
# add_all_miss
# add_any_complete
# add_all_complete
# which take variables
# these would be built off of the functions `any_miss` and `all_miss` and their
# complements

# all(is.na(x))
# all(!is.na(x))
# any(is.na(x))
# any(!is.na(x))

# but they would return "missing" or "complete"
# they would also work row-wise, for the selected variables

