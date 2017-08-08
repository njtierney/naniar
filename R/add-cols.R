#' Add a shadow column to dataframe
#'
#' As an alternative to `bind_shadow()`, you can add specific individual shadow
#'   columns to a dataset. These also respect the dplyr verbs
#'   `starts_with`, `contains`, `ends_with`, etc.
#'
#' @param data data.frame
#' @param ... One or more unquoted variable names separated by commas. These also
#'   respect the dplyr verbs `starts_with`, `contains`, `ends_with`, etc.
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' airquality %>% add_shadow(Ozone)
#' airquality %>% add_shadow(Ozone, Solar.R)
#'
add_shadow <- function(data, ...){

  if (missing(...)) {
    stop("please include variables to be selected after the data")
  } else {

    quo_vars <- rlang::quos(...)

    shadow_df <- dplyr::select(data, !!!quo_vars) %>% as_shadow()

    dplyr::bind_cols(data, shadow_df) %>% dplyr::as_tibble()

  } # close else statement

}

#' Add a shadow shifted column to a dataset
#'
#' Shadow shift only the selected variables in a dataset by specifying variable
#'   names or use dplyr `vars` and dplyr verbs `starts_with`, `contains`,
#'   `ends_with`, etc.
#'
#' @param data data.frame or .tbl
#' @param ... One or more unquoted variable names separated by commas. These also
#'   respect the dplyr verbs `starts_with`, `contains`, `ends_with`, etc.
#' @param suffix suffix to add to variable, defaults to "shift"
#'
#' @return data with the added variable shifted named as `var_suffix`
#'
#' @export
#'
#' @examples
#'
#' pedestrian %>% add_shadow_shift(hourly_counts)
#'
#' airquality %>% add_shadow_shift(Ozone, Solar.R)
#'
add_shadow_shift <- function(data, ..., suffix = "shift"){

  # if no variables are selected use all of the variables
  if (missing(...)) {

    shadow_shifted_df <- purrr::map_df(data, shadow_shift)

    # change names
    names(shadow_shifted_df) <- paste0(names(shadow_shifted_df), "_", suffix)

    tibble::as_tibble(dplyr::bind_cols(data, shadow_shifted_df))

  } else {

  # select variables
    quo_vars <- rlang::quos(...)

    shadow_shifted_vars <- dplyr::select(data, !!!quo_vars)

  # shadow shift all (using purrr:map_df)
    # would be good to have a way of indicating that no shift was taken at all
    shadow_shifted_df <- purrr::map_df(shadow_shifted_vars, shadow_shift)

  # change names
  names(shadow_shifted_df) <- paste0(names(shadow_shifted_df),"_",suffix)

  tibble::as_tibble(dplyr::bind_cols(data, shadow_shifted_df))
  } # close the else brace
}

#' Add a column describing presence of any missing values
#'
#' This adds a column named "any_na" (by default) that describes whether any
#'   there are any missings in all of the variables (default), or whether any of
#'   the specified columns, specified using variables names or dplyr verbs,
#'   `starts_with`, `contains`, `ends_with`, etc. By default the added column
#'    will be called "any_na_all", if no variables are specified, otherwise,
#'    if variables are specified, the label will be "any_na_vars" to indicate
#'    that not all variables have been used to create the labels. By default the
#'    `label` argument uses the prefix "any_na", but this can be specified.
#'
#' @param data data.frame
#' @param ... Variable names to use instead of the whole dataset. This can be
#'   one or more unquoted variable names separated by commas. These also
#'   respect the dplyr verbs `starts_with`, `contains`, `ends_with`, etc.
#' @param label label for the column, defaults to "any_na". By default if no
#'   additional variables are listed the label col is "any_na_all", otherwise
#'   it is "any_na_vars", if variables are specified.
#'
#' @return data.frame with data and the column labelling whether that row (for
#'     those variables) has any missing values - indicated by "missing" and
#'     "complete".
#'
#' @export
#'
#' @examples
#'
#' airquality %>% add_any_na()
#' airquality %>% add_any_na(Ozone, Solar.R)
#'
add_any_na <- function(data, ..., label = "any_na"){

  # if no variables are specified, do for all, and add the label "all"
  if (missing(...)) {

    stub_data_label <- data %>%
      dplyr::mutate(.temp = any_row_na(data),
                    .temp_label = dplyr::if_else(condition = .temp == TRUE,
                                                 true = "missing",
                                                 false = "complete")) %>%
      dplyr::select(.temp_label) %>%
      tibble::as_tibble()

    names(stub_data_label) <- paste0(label,"_all")

    dplyr::bind_cols(data, stub_data_label) %>% tibble::as_tibble()

  } else {

  quo_vars <- rlang::quos(...)

  stub_data <- dplyr::select(data, !!!quo_vars)

  stub_data_label <- stub_data %>%
    dplyr::mutate(.temp = any_row_na(stub_data),
                  .temp_label = dplyr::if_else(condition = .temp == TRUE,
                                               true = "missing",
                                               false = "complete")) %>%
    dplyr::select(.temp_label) %>%
    tibble::as_tibble()

  names(stub_data_label) <- paste0(label,"_vars")

  dplyr::bind_cols(data, stub_data_label) %>% tibble::as_tibble()

  }

}

#' Is there a missing value in the row of a dataframe?
#'
#' Creates a character vector describing presence/absense of missing values
#'
#' @param data a dataframe or set of vectors of the same length
#'
#' @return character vector of "Missing" and "Not Missing".
#'
#' @export
#'
#' @examples
#'
#' label_missings(airquality)
#'
#' library(dplyr)
#'
#' airquality %>% mutate(is_missing = label_missings(airquality))
#'
label_missings <- function(data){

  test_if_null(data)
  # find which are missing and which are not.

  any_row_na <- function(x){
    apply(data.frame(x), MARGIN = 1, FUN = function(x) anyNA(x))
  }

  temp <- any_row_na(data)

  dplyr::if_else(condition = temp == TRUE, # TRUE means missing
                 true = "Missing",
                 false = "Not Missing")

}

#' Add a column describing if there are any missings in the dataset
#'
#' @param data data.frame
#'
#' @return data.frame with a column "any_missing" that is either "Not Missing" or "Missing" for the purposes of plotting / exploration / nice print methods
#' @export
#'
#' @examples
#'
#' airquality %>% add_label_missings()
#'
add_label_missings <- function(data){

  data %>%
    dplyr::mutate(any_missing = label_missings(.)) %>%
    dplyr::as_tibble()

}

#' Label shadow values as missing or not missing
#'
#' Powers `add_label_shadow`. For the moment it is an internal function.
#'
#' @param data data.frame
#'
#' @return "Missing" or "Not Missing"
#'
label_shadow <- function(data){

# It is called "shade" because if you are in a shadow, you are in the shade.
# this may be helpful if shadows are their own class / have special factor
# attributes, then all you need is to test to see if they are of a class.

  any_shade <- function(x) any(grepl("^NA|^NA_", x))

  any_row_shade <- function(x){
    apply(data.frame(x), MARGIN = 1, FUN = function(x) any_shade(x))
  }

  temp <- any_row_shade(data)
    dplyr::if_else(condition = temp == TRUE, # TRUE means missing
                   true = "Missing",
                   false = "Not Missing")

}

#' Add a column describing whether there is a shadow
#'
#' Instead of focussing on labelling whether there are missings, we instead
#'   focus on whether there have been any shadows created. This can be useful
#'   when data has been imputed and you need to determine which rows contained
#'   missing values when the shadow was bound to the dataset.
#'
#' @param data data.frame
#'
#' @return data.frame with a column, "any_missing", which describes whether or
#'   not there are any rows that have a shadow value.
#'
#' @export
#'
#' @examples
#'
#' airquality %>%
#'   add_shadow(Ozone, Solar.R) %>%
#'   add_label_shadow()
#'
add_label_shadow <- function(data){

  data %>%
    dplyr::mutate(any_missing = label_shadow(.))

}

airquality %>%
  dplyr::mutate(any_missing = label_shadow(.))

label_shadow(airquality)
