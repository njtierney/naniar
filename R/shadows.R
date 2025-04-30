#' Create shadows
#'
#' Return a tibble in shadow matrix form, where the variables are the same but
#' have a suffix _NA attached to distinguish them.
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
#' @examples
#'
#' as_shadow(airquality)
#' @export

as_shadow <- function(data, ...) {
  test_if_null(data)

  test_if_dataframe(data)

  data_shadow <- purrr::map_dfc(data, shade)

  names(data_shadow) <- paste0(names(data), "_NA")

  return(data_shadow)
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
#' \dontrun{
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
#' }
#'
bind_shadow <- function(data, only_miss = FALSE, ...) {
  # If you want only the missing values to be added
  if (only_miss) {
    # I want to only select columns that contain a missing value.
    miss_vars <- rlang::syms(miss_var_which(data))

    shadow_vars <- dplyr::as_tibble(as_shadow(dplyr::select(
      data,
      !!!miss_vars
    )))
    data <- tibble::as_tibble(data)
    shadow_data <- dplyr::bind_cols(data, shadow_vars)

    return(shadow_data)
  }

  if (!only_miss) {
    data_shadow <- tibble::as_tibble(as_shadow(data))
    data <- tibble::as_tibble(data)
    shadow_data <- dplyr::bind_cols(data, data_shadow)

    if (!missing(...)) {
      shadow_data <- shadow_data %>% recode_shadow(...)
    }

    return(shadow_data)
  }
}

#' Unbind (remove) shadow from data, and vice versa
#'
#' Remove the shadow variables (which end in `_NA`) from the data, or vice versa.
#' This will also remove the `nabular` class from the data.
#'
#' @param data data.frame containing shadow columns (created by [bind_shadow()])
#'
#' @return `data.frame` without shadow columns if using [unbind_shadow()], or
#'   without the original data, if using [unbind_data()].
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
unbind_shadow <- function(data) {
  test_if_any_shade(data)
  temp <- dplyr::select(data, -dplyr::ends_with("_NA"))
  return(temp)
}

#' @rdname unbinders
#' @export
unbind_data <- function(data) {
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
gather_shadow <- function(data) {
  as_shadow(data) %>%
    dplyr::mutate(rows = seq_len(nrow(.))) %>%
    tidyr::pivot_longer(
      cols = -rows,
      names_to = "variable",
      values_to = "missing"
    ) %>%
    dplyr::rename(case = rows)
}


#' Reshape shadow data into a long format
#'
#' Once data is in `nabular` form, where the shadow is bound to the data, it
#'     can be useful to reshape it into a long format with the shadow columns
#'     in a separate grouping - so you have `variable`, `value`, and
#'     `variable_NA` and `value_NA`.
#'
#' @param shadow_data a data.frame
#' @param ... bare name of variables that you want to focus on
#' @param fn_value_transform function to transform the "value" column. Default
#'   is NULL, which defaults to `as.character`. Be aware that `as.numeric` may
#'   fail for some instances if it cannot coerce the value into numeric. See
#'   the examples.
#' @param only_main_vars logical - do you want to filter down to main variables?
#'
#' @return data in long format, with columns `variable`, `value`, `variable_NA`, and `value_NA`.
#' @export
#'
#' @examples
#'
#' aq_shadow <- nabular(airquality)
#'
#' shadow_long(aq_shadow)
#'
#' # then filter only on Ozone
#' shadow_long(aq_shadow, Ozone)
#'
#' shadow_long(aq_shadow, Ozone, Solar.R)
#'
#' # ensure `value` is numeric
#' shadow_long(aq_shadow, fn_value_transform = as.numeric)
#' shadow_long(aq_shadow, Ozone, Solar.R, fn_value_transform = as.numeric)
#'
#'
shadow_long <- function(
  shadow_data,
  ...,
  fn_value_transform = NULL,
  only_main_vars = TRUE
) {
  test_if_null(shadow_data)
  test_if_any_shade(shadow_data)

  if (is.null(fn_value_transform)) {
    fn_value_transform <- as.character
  }

  shadow_data_names <- names(which_are_shade(shadow_data))
  longer_one <- tidyr::pivot_longer(
    shadow_data,
    cols = -dplyr::one_of(shadow_data_names),
    names_to = "variable",
    values_to = "value",
    values_transform = list(value = fn_value_transform)
  )

  longer_one_shade_names <- names(which_are_shade(longer_one))
  gathered_df <- tidyr::pivot_longer(
    longer_one,
    cols = dplyr::one_of(longer_one_shade_names),
    names_to = "variable_NA",
    values_to = "value_NA"
  )

  if (only_main_vars) {
    gathered_df <- dplyr::filter(
      gathered_df,
      variable_NA == paste0(variable, "_NA")
    )
  }

  if (!missing(...)) {
    df_vars <- purrr::map_chr(ensyms(...), as_string)
    gathered_df <- gathered_df %>%
      dplyr::filter(variable %in% df_vars)
  }

  return(gathered_df)
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
as_shadow_upset <- function(data) {
  if (n_var_miss(data) <= 1) {
    if (n_var_miss(data) == 1) {
      glu_st <- glue::glue(
        "upset plots for missing data requre at least two \\
                         variables to have missing data, only one variable, \\
                         '{miss_var_which(data)}' has missing values."
      )
    }

    if (n_var_miss(data) == 0) {
      glu_st <- glue::glue(
        "upset plots for missing data requre at least two \\
                         variables to have missing data, there are no missing \\
                         values in your data! This is probably a good thing."
      )
    }

    rlang::abort(message = glu_st)
  }

  test_if_null(data)

  test_if_dataframe(data)

  data_shadow <- as.data.frame(is.na(data) * 1)

  names(data_shadow) <- paste0(names(data), "_NA")

  dplyr::mutate_if(data_shadow, is.numeric, as.integer)
}
