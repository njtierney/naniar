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
#' It can be useful when doing data analysis to add the number of missing data points into your dataframe. add_n_miss adds a column named "n_miss", which contains the number of missing values in that row.
#'
#' @param data a dataframe
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' airquality %>% add_n_miss()
#'
#'
add_n_miss <- function(data){

  purrrlyr::by_row(.d = data,
                ..f = function(x) n_miss(x),
                .collate = "row",
                .to = "n_miss")

  # old approach
  # # create a numeric vector of n_missing
  # col_n_miss <- data.frame(n_miss = apply(df,1,n_miss))
  #
  # dplyr::bind_cols(df,col_n_miss)

}

#' Add column containing proportion of missing data values
#'
#' It can be useful when doing data analysis to add the proportion of missing data values into your dataframe. add_prop_miss adds a column named "prop_miss", which contains the proportion of missing values in that row.
#'
#' @param data a dataframe
#' @param vars character string of variable names, default is all variables
#' @param label character string of what you need to name variable
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#'
#' add_prop_miss(airquality)
#'
#' add_prop_miss(airquality, "Month", label = "testing")
#'
#' add_prop_miss(airquality, "Month")
#'
#' # this can be applied to model the proportion of missing data
#' # as in Tierney et al bmjopen.bmj.com/content/5/6/e007450.full
#' library(rpart)
#' library(rpart.plot)
#'
#' airquality %>%
#' add_prop_miss() %>%
#' rpart(prop_miss ~ ., data = .) %>%
#' prp(type = 4,
#'     extra = 101,
#'     prefix = "prop_miss = ")


add_prop_miss <- function(data, vars = NULL, label = "prop_miss"){

  if(is.null(vars)){
    purrrlyr::by_row(.d = data,
                     ..f = function(x) (mean(is.na(x))),
                     .collate = "row",
                     .to = "prop_miss")
  } else {

  quo_vars <- rlang::quos(vars)

  selected_data <- dplyr::select(data, !!!quo_vars)

  prop_selected_data <- purrrlyr::by_row(.d = selected_data,
                                         ..f = function(x) (mean(is.na(x))),
                                         .collate = "row",
                                         .to = label)

  prop_selected_data
  # unsure if this should return just the variables computed -----
    # perhaps add a label to this to sescribe which variable were used in its calculation?
    # dplyr::select(!!as.name(label))
  # dplyr::as_tibble(dplyr::bind_cols(data, prop_selected_data))

  # old approach
  # df %>%
  #   add_n_miss() %>%
  #   dplyr::mutate(pct_miss = n_miss/ncol(df)) %>%
  #   dplyr::select(-n_miss)
  }
}

#' Add a shadow shifted column to a dataset
#'
#' Shifting the values of a numeric
#'
#' @param data data.frame or .tbl
#' @param vars quoted variables that you want to shift
#' @param suffix suffix to add to variable, defaults to "shift"
#'
#' @return .data with the added variable shifted named as `var_suffix`
#'
#' @export
#'
#' @examples
#'
#' add_shadow_shift(pedestrian, vars = "hourly_counts")
#' add_shadow_shift(airquality, vars = c("Ozone", "Solar.R"))
#'
add_shadow_shift <- function(data, vars, suffix = "shift"){

  # select variables
    quo_vars <- rlang::quos(vars)

    shadow_shifted_vars <- dplyr::select(data, !!!quo_vars)

  # shadow shift all (using purrr:map_df)
    # would be good to have a way of indicating that no shift was taken at all
    shadow_shifted_df <- purrr::map_df(shadow_shifted_vars, shadow_shift)

  # change names
  names(shadow_shifted_df) <- paste0(names(shadow_shifted_df),"_",suffix)

  tibble::as_tibble(dplyr::bind_cols(data, shadow_shifted_df))

}

#' Add a shadow column to a dataset
#'
#' Shifting the values to make them easier to display
#'
#' @param data data.frame or .tbl
#' @param vars quoted variablename
#'
#' @return .data with the added variable shifted named as `var_NA`
#'
#' @export
#'
#' @examples
#'
#' airquality %>% cast_shadow("Ozone")
#'
cast_shadow <- function(data, vars){

  quo_vars <- rlang::quos(vars)

  # shadow all (using purrr:map_df)
  shadow_vars <- dplyr::select(data, !!!quo_vars) %>% as_shadow
  # cannot get this to take multiple variables
  # shadow_vars <- dplyr::select(data, .data[[vars]]) %>% as_shadow

  my_data <- dplyr::select(data, !!!quo_vars)

  tibble::as_tibble(dplyr::bind_cols(my_data, shadow_vars))

}

#' Add a shadow and a shadow_shift column to a dataset
#'
#' Shift the values and add the shadow
#'
#' @param data data.frame
#' @param vars character string for variables
#'
#' @return data.frame with the shadow and shadow_shift vars
#'
#' @export
#'
#' @examples
#'
#' airquality %>% cast_shadow_shift("Ozone")
#' airquality %>% cast_shadow_shift(c("Ozone", "Temp"))
#'
cast_shadow_shift <- function(data, vars){

  quo_vars <- rlang::quos(vars)

  shadow_vars <- dplyr::select(data, !!!quo_vars) %>% cast_shadow(vars)

  # shift those values selected
  add_shadow_shift(shadow_vars, vars)

}

#' Add a shadow and a shadow_shift column to a dataset
#'
#' Shift the values, add shadow, add missing label
#'
#' @param data data.frame
#' @param vars character string for variables
#'
#' @return data.frame with the shadow and shadow_shift vars, and missing labels
#' @export
#'
#' @examples
#'
#' airquality %>% cast_shadow_shift_label("Ozone")
#' airquality %>% cast_shadow_shift_label(c("Ozone", "Solar.R"))
#'
cast_shadow_shift_label <- function(data, vars){

  quo_vars <- rlang::quos(vars)

  shadow_vars <- dplyr::select(data, !!!quo_vars) %>% cast_shadow(vars)

  # shift those values selected
  add_shadow_shift(shadow_vars, vars) %>% add_label_missings()

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
#' @param vars quoted variables
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
#' add_any_miss(airquality, c("Ozone", "Solar.R"))
#'
add_any_miss <- function(data, vars, label = "any_miss"){

  quo_vars <- rlang::quos(vars)

  stub_data <- dplyr::select(data, !!!quo_vars)

  stub_data_label <- stub_data %>%
    dplyr::mutate(.temp = any_row_miss(stub_data),
                  .temp_label = dplyr::if_else(condition = .temp == TRUE,
                                               true = "missing",
                                               false = "complete")) %>%
    dplyr::select(.temp_label) %>%
    tibble::as_tibble()

  names(stub_data_label) <- label

  dplyr::bind_cols(data, stub_data_label) %>% tibble::as_tibble()

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
    dplyr::mutate(any_missing = label_missings(.))

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
#' add_shadow(c("Ozone", "Solar.R")) %>%
#' add_label_shadow()

#'
add_label_shadow <- function(data){

  data %>%
    dplyr::mutate(any_missing = label_shadow(.))

}



#' Add a column of the shadows to the dataframe
#'
#' @param data data.frame
#' @param vars quoted character string
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' airquality %>% add_shadow(c("Ozone", "Solar.R"))
#'
add_shadow <- function(data, vars){

  quo_vars <- rlang::quos(vars)

  shadow_df <- dplyr::select(data, !!!quo_vars) %>% as_shadow()

  dplyr::bind_cols(data, shadow_df) %>% dplyr::as_tibble()

}
