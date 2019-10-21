#' Create shadows
#'
#' Representing missing data structure is achieved using the shadow matrix,
#' introduced in [Swayne and Buja](https://www.researchgate.net/publication/2758672_Missing_Data_in_Interactive_High-Dimensional_Data_Visualization). The shadow
#' matrix is the same dimension as the data, and consists of binary indicators
#' of missingness of data values, where missing is represented as "NA", and not
#' missing is represented as "!NA". Although these may be represented as 1 and
#' 0, respectively.
#'
#' @param data dataframe
#' @param ... selected variables to use
#'
#' @return appended shadow with column names
#' @export

as_shadow <- function(data, ...){

  test_if_null(data)

  test_if_dataframe(data)

  UseMethod("as_shadow")
}

#' Create shadow data
#'
#' Return a tibble in shadow matrix form, where the variables are the same but
#' have a suffix _NA attached to distinguish them.
#'
#' @inheritParams as_shadow
#'
#' @examples
#'
#' as_shadow(airquality)
#'
#' @export
as_shadow.data.frame <- function(data, ...){

  data_shadow <- purrr::map_dfc(data, shade)

  names(data_shadow) <- paste0(names(data),"_NA")


  return(new_shadow(data_shadow))

}


#' Convert data into shadow format for doing an upset plot
#'
#' Upset plots are a way of visualising common sets, this function transforms
#'     the data into a format that feeds directly into an upset plot
#'
#' @param data a data.frame
#'
#' @return a data.frame
#'
#' @examples
#'
#' \dontrun{
#'
#' library(UpSetR)
#' airquality %>%
#'   as_shadow_upset() %>%
#'   upset()
#' }
#'
#' @export
as_shadow_upset <- function(data){

  test_if_null(data)

  test_if_dataframe(data)

  data_shadow <- as.data.frame(is.na(data)*1)

  names(data_shadow) <- paste0(names(data),"_NA")

  dplyr::mutate_if(data_shadow, is.numeric, as.integer)

}


#' Bind a shadow dataframe to original data
#'
#' Binding a shadow matrix to a regular dataframe helps visualise and work with
#' missing data.
#'
#' @param data a dataframe
#' @param only_miss logical - if FALSE (default) it will bind a dataframe with
#'     all of the variables duplicated with their shadow. Setting this to TRUE
#'     will bind variables only those variables that contain missing values.
#'     See the examples for more details.
#' @param ... extra options to pass to [recode_shadow()] - a work in progress.
#'
#' @return data with the added variable shifted and the suffix `_NA`
#' @export
#'
#' @examples
#'
#' bind_shadow(airquality)
#'
#' # bind only the variables that contain missing values
#' bind_shadow(airquality, only_miss = TRUE)
#'
#' aq_shadow <- bind_shadow(airquality)
#'
#' # explore missing data visually
#' library(ggplot2)
#'
#' # using the bounded shadow to visualise Ozone according to whether Solar
#' # Radiation is missing or not.
#'
#' ggplot(data = aq_shadow,
#'        aes(x = Ozone)) +
#'        geom_histogram() +
#'        facet_wrap(~Solar.R_NA,
#'        ncol = 1)
#'
bind_shadow <- function(data, only_miss = FALSE, ...){

  # If you want only the missing values to be added
  if (only_miss) {

    # I want to only select columns that contain a missing value.
    miss_vars <- rlang::syms(miss_var_which(data))

    shadow_vars <- dplyr::select(data, !!!miss_vars) %>% as_shadow()

    shadow_data <- tibble::as_tibble(dplyr::bind_cols(data, shadow_vars))

    # class(shadow_data) <- c("shadow", class(shadow_data))

    # return(new_shadow(shadow_data))
    return(new_nabular(shadow_data))

  # if you want All the values to be added (the default behaviour)
  }

  if (!only_miss) {

    data_shadow <- as_shadow(data)

    bound_shadow <- dplyr::bind_cols(data, data_shadow)

    shadow_data <- tibble::as_tibble(bound_shadow)

    if (!missing(...)) {
      shadow_data <- shadow_data %>% recode_shadow(...)
    }

    # class(shadow_data) <- c("shadow", class(shadow_data))

    # return(new_shadow(shadow_data))
    return(new_nabular(shadow_data))

  }

}

#' Create a new shadow
#'
#' @param x a data.frame
#'
#' @return object with class "shadow", inheriting from it's original class
#' @export
new_shadow <- function(x){
  tibble::new_tibble(x, subclass = "shadow", nrow = as.integer(nrow(x)))
}


#' Unbind (remove) shadow from data, and vice versa
#'
#' Remove the shadow variables (which end in _NA) from the data, or vice versa
#'
#' @param data a data.frame containing shadow columns (created by bind_shadow)
#'
#' @return dataframe without shadow columns if using unbind_shadow, or without
#'  the original data, if using unbind_data
#' @name unbinders
#'
#' @export
#'
#' @examples
#'
#' # bind shadow columns
#' aq_sh <- bind_shadow(airquality)
#'
#' # print data
#' aq_sh
#'
#' # remove shadow columns
#' unbind_shadow(aq_sh)
#'
#' # remove data
#' unbind_data(aq_sh)
#'
#' # errors when you don't use data with shadows
#' \dontrun{
#'  unbind_data(airquality)
#'  unbind_shadow(airquality)
#' }
#'
unbind_shadow <- function(data){
  test_if_any_shade(data)
  dplyr::select(data, -dplyr::ends_with("_NA"))
}

#' @rdname unbinders
#' @export
unbind_data <- function(data){
  test_if_any_shade(data)
  dplyr::select(data, dplyr::ends_with("_NA"))
}


#' Long form representation of a shadow matrix
#'
#' `gather_shadow` is a long-form representation of binding the shadow matrix to
#'     your data, producing variables named `case`, `variable`, and `missing`, where
#'     `missing` contains the missing value representation.
#'
#' @param data a dataframe
#'
#' @return dataframe in long, format, containing information about the missings
#'
#' @export
#'
#' @examples
#'
#' gather_shadow(airquality)
#'
gather_shadow <- function(data){

  as_shadow(data) %>%
    dplyr::mutate(rows = seq_len(nrow(.))) %>%
    tidyr::gather(key = "variable",
                  value = "missing",
                  -rows) %>%
    dplyr::rename(case = rows)
}


# #' Is this thing a shadow?
# #'
# #' Does this thing contain a shadow variable?
# #'
# #' @param x vector or data.frame
# #'
# #' @return logical - single value. TRUE if contains a variable with a column ending in "_NA"
# #' @export
# #'
# #' @examples
# #'
# #' df_shadow <- bind_shadow(airquality)
# #'
# #' is_shadow(df_shadow)
# #'
# #' @export
# is_shadow <- function(x){
#   # any(grepl("_NA",names(x)))
#   inherits(x, "shadow")
# }
#
# #' Are these things shadows?
# #'
# #' Does this thing contain a shadow variable?
# #'
# #' @param x vector or data.frame
# #'
# #' @return logical vector - TRUE if contains a variable with a column ending in "_NA"
# #' @export
# #'
# #' @examples
# #'
# #' df_shadow <- bind_shadow(airquality)
# #'
# #' are_shadow(df_shadow)
# #'
# #' @export
# are_shadow <- function(x){
#   grepl("_NA",names(x))
#   purrr::map(x, class) %>%
#     tibble::as_tibble() %>%
# }

# Are these things shadows?
#
# Does this thing contain a shadow variable?
#
# @param x vector or data.frame
#
# @return logical vector - TRUE if contains a variable with a column ending in "_NA"
# @export
#
# @examples
#
# df_shadow <- bind_shadow(airquality)
#
# are_shadow(df_shadow)
#
#@export
# are_shadow <- function(x) grepl("_NA",names(x))


#' Which variables are shades?
#'
#' This function tells us which variables contain shade information
#'
#' @param .tbl a data.frame or tbl
#'
#' @return numeric - which column numbers contain shade information
#'
#' @examples
#'
#' df_shadow <- bind_shadow(airquality)
#'
#' which_are_shade(df_shadow)
#'
#' @export
which_are_shade <- function(.tbl){
  test_if_null(.tbl)
  test_if_dataframe(.tbl)
  which(are_shade(.tbl))
}


#' Reshape shadow data into a long format
#'
#' Once data is in `nabular` form, where the shadow is bound to the data, it
#'     can be useful to reshape it into a long format with the columns
#'
#' @param shadow_data a data.frame
#' @param ... bare name of variables that you want to focus on
#' @param only_main_vars logical - do you want to filter down to main variables?
#'
#' @return data in long format, with columns `variable`, `value`, `variable_NA`, and `value_NA`.
#' @export
#'
#' @examples
#'
#' aq_shadow <- bind_shadow(airquality)
#'
#' shadow_long(aq_shadow)
#'
#' # then filter only on Ozone
#' shadow_long(aq_shadow, Ozone)
#'
#' shadow_long(aq_shadow, Ozone, Solar.R)
#'
#'
shadow_long <- function(shadow_data,
                        ...,
                        only_main_vars = TRUE){

  test_if_null(shadow_data)
  test_if_any_shade(shadow_data)

  gathered_df <- shadow_data %>%
    tidyr::gather(key = "variable",
                  value = "value",
                  -which_are_shade(.)) %>%
    tidyr::gather(key = "variable_NA",
                  value = "value_NA",
                  which_are_shade(.))

  if (only_main_vars) {
    gathered_df <- dplyr::filter(gathered_df,
                                 variable_NA == paste0(variable,"_NA"))
  }

  if (!missing(...)) {
    vars <- purrr::map(ensyms(...), as_string)
    gathered_df <- gathered_df %>%
      dplyr::filter(variable %in% vars)
  }

    return(gathered_df)

}
