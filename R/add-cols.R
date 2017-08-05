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
#' add_span_counter(pedestrian, span_size = 100)
#' }
add_span_counter <- function(data, span_size) {

  dplyr::mutate(data,
                span_counter = rep(x = 1:ceiling(nrow(data)),
                                   each = span_size,
                                   length.out = nrow(data)))
}

#' Add column containing number of missing data values
#'
#' It can be useful when doing data analysis to add the number of missing data
#'   points into your dataframe. `add_n_miss` adds a column named "n_miss",
#'   which contains the number of missing values in that row.
#'
#' @param data a dataframe
#' @param ... Variable names to use instead of the whole dataset. By default this
#'   looks at the whole dataset. Otherwise, this is one or more unquoted
#'   expressions separated by commas. These also respect the dplyr verbs
#'   "starts_with", "contains", "ends_with", etc. By default will add "_all" to
#'   the label if left blank, otherwise will add "_vars" to distinguish that it
#'   has not been used on all of the variables.
#' @param label character default is "n_miss".
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#'
#' airquality %>% add_n_miss()
#' airquality %>% add_n_miss(Ozone, Solar.R)
#' airquality %>% add_n_miss(dplyr::contains("o"))
#'
#'
add_n_miss <- function(data, ..., label = "n_miss"){

  if (missing(...)) {
    purrrlyr::by_row(.d = data,
                     ..f = function(x) n_miss(x),
                     .collate = "row",
                     .to = paste0(label,"_all"))
  } else {

    quo_vars <- rlang::quos(...)

    selected_data <- dplyr::select(data, !!!quo_vars)

    prop_selected_data <- purrrlyr::by_row(.d = selected_data,
                                           ..f = function(x) n_miss(x),
                                           .collate = "row",
                                           .to =  paste0(label,"_vars"))

    # add only the variables prop_miss function, not the whole data.frame...
    prop_selected_data_cut <- prop_selected_data %>%
      dplyr::select(!!as.name(paste0(label,"_vars")))

    dplyr::bind_cols(data, prop_selected_data_cut) %>% dplyr::as_tibble()

  } # close else loop

}

#' Add column containing proportion of missing data values
#'
#' It can be useful when doing data analysis to add the proportion of missing
#'   data values into your dataframe. `add_prop_miss` adds a column named
#'   "prop_miss", which contains the proportion of missing values in that row.
#'   You can specify the variables that you would like to show the missingness
#'   for.
#'
#' @param data a dataframe
#' @param ... Variable names to use instead of the whole dataset. By default this
#'   looks at the whole dataset. Otherwise, this is one or more unquoted
#'   expressions separated by commas. These also respect the dplyr verbs
#'   `starts_with`, `contains`, `ends_with`, etc. By default will add "_all" to
#'   the label if left blank, otherwise will add "_vars" to distinguish that it
#'   has not been used on all of the variables.
#' @param label character string of what you need to name variable
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#'
#' airquality %>% add_prop_miss()
#'
#' airquality %>% add_prop_miss(Solar.R)
#'
#' airquality %>% add_prop_miss(Solar.R, Ozone)
#'
#' airquality %>% add_prop_miss(Solar.R, Ozone, label = "testing")
#'
#' # this can be applied to model the proportion of missing data
#' # as in Tierney et al bmjopen.bmj.com/content/5/6/e007450.full
#' library(rpart)
#' library(rpart.plot)
#'
#' airquality %>%
#' add_prop_miss() %>%
#' rpart(prop_miss_all ~ ., data = .) %>%
#' prp(type = 4,
#'     extra = 101,
#'     prefix = "prop_miss = ")

add_prop_miss <- function(data, ..., label = "prop_miss"){

  if (missing(...)) {
    purrrlyr::by_row(.d = data,
                     ..f = function(x) (mean(is.na(x))),
                     .collate = "row",
                     .to = paste0(label,"_all"))
  } else {

  quo_vars <- rlang::quos(...)

  selected_data <- dplyr::select(data, !!!quo_vars)

  prop_selected_data <- purrrlyr::by_row(.d = selected_data,
                                         ..f = function(x) prop_miss(x),
                                         .collate = "row",
                                         .to =  paste0(label,"_vars"))

  # add only the variables prop_miss function, not the whole data.frame...
  prop_selected_data_cut <- prop_selected_data %>%
    dplyr::select(!!as.name(paste0(label,"_vars")))

  dplyr::bind_cols(data, prop_selected_data_cut) %>% dplyr::as_tibble()

  # old approach
  # df %>%
  #   add_n_miss() %>%
  #   dplyr::mutate(pct_miss = n_miss/ncol(df)) %>%
  #   dplyr::select(-n_miss)
  }
}

#' Add a shadow shifted column to a dataset
#'
#' Shadow shift only the selected variables in a dataset using dplyr `vars`
#'   and dplyr verbs "starts_with", "contains", "ends_with", etc.
#'
#' @param data data.frame or .tbl
#' @param ... One or more unquoted expressions separated by commas. These also
#'   respect the dplyr verbs "starts_with", "contains", "ends_with", etc.
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
  if(missing(...)){

    shadow_shifted_df <- purrr::map_df(data, shadow_shift)

    # change names
    names(shadow_shifted_df) <- paste0(names(shadow_shifted_df),"_",suffix)

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

# UP TO HERE

#' Add a shadow column to a dataset
#'
#' Shifting the values to make them easier to display
#'
#' @param data data.frame
#' @param ... One or more unquoted expressions separated by commas. These also
#'   respect the dplyr verbs "starts_with", "contains", "ends_with", etc.
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

  # shadow all (using purrr:map_df)
  shadow_vars <- dplyr::select(data, !!!quo_vars) %>% as_shadow()
  # cannot get this to take multiple variables
  # shadow_vars <- dplyr::select(data, .data[[vars]]) %>% as_shadow

  my_data <- dplyr::select(data, !!!quo_vars)

  tibble::as_tibble(dplyr::bind_cols(my_data, shadow_vars))

  } # close else loop

}

#' Add a shadow and a shadow_shift column to a dataset
#'
#' Shift the values and add the shadow
#'
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

#' Add a shadow and a shadow_shift column to a dataset
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


#' Add a column that tells you if there are any missing values
#'
#' @param data data.frame
#' @param ... Variable names to use instead of the whole dataset. By default this
#'   looks at the whole dataset. Otherwise, this is one or more unquoted
#'   expressions separated by commas. These also respect the dplyr verbs
#'   "starts_with", "contains", "ends_with", etc. By default will add "_all" to
#'   the label if left blank, otherwise will add "_vars" to distinguish that it
#'   has not been used on all of the variables.
#' @param label label for the column, defaults to "any_miss"
#'
#' @return data.frame with data and the column labelling whether that row (for
#'     those variables) has any missing values - indicated by "missing" and
#'     "complete".
#'
#' @note perhaps there should be a general function for this
#'
#' @export
#'
#' @examples
#'
#' airquality %>% add_any_miss()
#' airquality %>% add_any_miss(Ozone, Solar.R)
#'
add_any_miss <- function(data, ..., label = "any_miss"){

  # if no variables are specified, do for all, and add the label "all"
  if(missing(...)){

    stub_data_label <- data %>%
      dplyr::mutate(.temp = any_row_miss(data),
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
    dplyr::mutate(.temp = any_row_miss(stub_data),
                  .temp_label = dplyr::if_else(condition = .temp == TRUE,
                                               true = "missing",
                                               false = "complete")) %>%
    dplyr::select(.temp_label) %>%
    tibble::as_tibble()

  names(stub_data_label) <- paste0(label,"_vars")

  dplyr::bind_cols(data, stub_data_label) %>% tibble::as_tibble()

  }

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
#' This is used to power add_label_shadow. It may be exported later, but for the
#'   moment this is an internal function
#'
#' @param data data.frame
#'
#' @return "Missing" or "Not Missing"
#'
label_shadow <- function(data){

# it is called a shade because if you are IN a shadow, then you are IN the shade
# might also possibly need to work with factors
# this might be helpful if the shadows are their own class or have special
# factor attributes, then all you need is to to a test to see if they are of
# that class?

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
#' @param data data.frame
#'
#' @return data.frame with
#'
#' @export
#'
#' @examples
#'
#' airquality %>%
#' add_shadow(Ozone, Solar.R) %>%
#' add_label_shadow()

#'
add_label_shadow <- function(data){

  data %>%
    dplyr::mutate(any_missing = label_shadow(.))

}

#' Add a column of the shadows to the dataframe
#'
#' @param data data.frame
#' @param ... One or more unquoted expressions separated by commas. These also
#'   respect the dplyr verbs "starts_with", "contains", "ends_with", etc.
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
