#' Add a shadow column to dataframe
#'
#' As an alternative to `bind_shadow()`, you can add specific individual shadow
#'   columns to a dataset. These also respect the dplyr verbs
#'   `starts_with`, `contains`, `ends_with`, etc.
#'
#' @param data data.frame
#' @param ... One or more unquoted variable names, separated by commas. These also
#'   respect the dplyr verbs `starts_with`, `contains`, `ends_with`, etc.
#'
#' @return data.frame
#' @export
#'
#' @seealso [bind_shadow()] [add_any_miss()] [add_label_missings()] [add_label_shadow()] [add_miss_cluster()] [add_n_miss()] [add_prop_miss()] [add_shadow_shift()] [cast_shadow()]
#'
#' @examples
#'
#' airquality %>% add_shadow(Ozone)
#' airquality %>% add_shadow(Ozone, Solar.R)
#'
add_shadow <- function(data, ...) {
  test_if_dots_missing(
    dots_empty = missing(...),
    msg = "{.fun add_shadow} requires variables to be selected"
  )
  shadow_df <- dplyr::select(data, ...) %>% as_shadow()

  data <- tibble::as_tibble(data)
  shadow_df <- tibble::as_tibble(shadow_df)
  dplyr::bind_cols(data, shadow_df)
}

#' Add a shadow shifted column to a dataset
#'
#' Shadow shift missing values using only the selected variables in a dataset,
#'   by specifying variable names or use dplyr `vars` and dplyr verbs
#'   `starts_with`, `contains`, `ends_with`, etc.
#'
#' @param data data.frame
#' @param ... One or more unquoted variable names separated by commas. These also
#'   respect the dplyr verbs `starts_with`, `contains`, `ends_with`, etc.
#' @param suffix suffix to add to variable, defaults to "shift"
#'
#' @return data with the added variable shifted named as `var_suffix`
#'
#' @export
#'
#' @seealso [bind_shadow()] [add_any_miss()] [add_label_missings()] [add_label_shadow()] [add_miss_cluster()] [add_n_miss()] [add_prop_miss()] [add_shadow_shift()] [cast_shadow()]
#'
#' @examples
#'
#' airquality %>% add_shadow_shift(Ozone, Solar.R)
#'
add_shadow_shift <- function(data, ..., suffix = "shift") {
  # if no variables are selected use all of the variables
  if (missing(...)) {
    shadow_shifted_df <- purrr::map_dfc(data, impute_below)

    # change names
    names(shadow_shifted_df) <- paste0(names(shadow_shifted_df), "_", suffix)

    data <- tibble::as_tibble(data)
    shadow_shifted_df <- tibble::as_tibble(shadow_shifted_df)

    return(dplyr::bind_cols(data, shadow_shifted_df))
  }

  # select variables
  shadow_shifted_vars <- dplyr::select(data, ...)

  # shadow shift all (using purrr:map_df)
  # would be good to have a way of indicating that no shift was taken at all
  shadow_shifted_df <- purrr::map_dfc(shadow_shifted_vars, impute_below)

  # change names
  names(shadow_shifted_df) <- paste0(names(shadow_shifted_df), "_", suffix)

  data <- tibble::as_tibble(data)
  shadow_shifted_df <- tibble::as_tibble(shadow_shifted_df)

  return(dplyr::bind_cols(data, shadow_shifted_df))
}

#' Add a column describing presence of any missing values
#'
#' This adds a column named "any_miss" (by default) that describes whether
#'   there are any missings in all of the variables (default), or whether any
#'   of the specified columns, specified using variables names or dplyr verbs,
#'   `starts_with`, `contains`, `ends_with`, etc. By default the added column
#'   will be called "any_miss_all", if no variables are specified, otherwise,
#'   if variables are specified, the label will be "any_miss_vars" to indicate
#'   that not all variables have been used to create the labels.
#'
#'
#'   By default the
#'   prefix "any_miss" is used, but this can be changed in the `label` argument.
#'
#' @param data data.frame
#' @param ...
#'
#' Variable names to use instead of the whole dataset. By default this
#'   looks at the whole dataset. Otherwise, this is one or more unquoted
#'   expressions separated by commas. These also respect the dplyr verbs
#'   `starts_with`, `contains`, `ends_with`, etc. By default will add "_all" to
#'   the label if left blank, otherwise will add "_vars" to distinguish that it
#'   has not been used on all of the variables.
#' @param label label for the column, defaults to "any_miss". By default if no
#'   additional variables are listed the label col is "any_miss_all", otherwise
#'   it is "any_miss_vars", if variables are specified.
#' @param missing character a label for when values are missing - defaults to "missing"
#' @param complete character character a label for when values are complete - defaults to "complete"
#'
#' @return data.frame with data and the column labelling whether that row (for
#'     those variables) has any missing values - indicated by "missing" and
#'     "complete".
#'
#' @export
#'
#' @seealso [bind_shadow()] [add_any_miss()] [add_label_missings()] [add_label_shadow()] [add_miss_cluster()] [add_n_miss()] [add_prop_miss()] [add_shadow_shift()] [cast_shadow()]
#'
#' @examples
#'
#' airquality %>% add_any_miss()
#' airquality %>% add_any_miss(Ozone, Solar.R)
#'
add_any_miss <- function(
  data,
  ...,
  label = "any_miss",
  missing = "missing",
  complete = "complete"
) {
  # if no variables are specified, do for all, and add the label "all"
  if (missing(...)) {
    stub_data_label <- data %>%
      dplyr::mutate(
        .temp = any_row_miss(data),
        .temp_label = dplyr::if_else(
          condition = .temp == TRUE,
          true = missing,
          false = complete
        )
      ) %>%
      dplyr::select(.temp_label) %>%
      tibble::as_tibble()

    names(stub_data_label) <- paste0(label, "_all")

    return(
      dplyr::bind_cols(data, stub_data_label) %>% tibble::as_tibble()
    )
  }

  stub_data <- dplyr::select(data, ...)

  stub_data_label <- stub_data %>%
    dplyr::mutate(
      .temp = any_row_miss(stub_data),
      .temp_label = dplyr::if_else(
        condition = .temp == TRUE,
        true = missing,
        false = complete
      )
    ) %>%
    dplyr::select(.temp_label) %>%
    tibble::as_tibble()

  names(stub_data_label) <- paste0(label, "_vars")

  dplyr::bind_cols(data, stub_data_label) %>% tibble::as_tibble()
}

#' Is there a missing value in the row of a dataframe?
#'
#' Creates a character vector describing presence/absence of missing values
#'
#' @param data a dataframe or set of vectors of the same length
#'
#' @return character vector of "Missing" and "Not Missing".
#' @param ... extra variable to label
#' @param missing character a label for when values are missing - defaults to "Missing"
#' @param complete character character a label for when values are complete - defaults to "Not Missing"
#'
#' @export
#'
#' @seealso [bind_shadow()] [add_any_miss()] [add_label_missings()] [add_label_shadow()] [add_miss_cluster()] [add_n_miss()] [add_prop_miss()] [add_shadow_shift()] [cast_shadow()]
#'
#' @examples
#'
#' label_missings(airquality)
#'
#' \dontrun{
#' library(dplyr)
#'
#' airquality %>%
#'   mutate(is_missing = label_missings(airquality)) %>%
#'   head()
#'
#' airquality %>%
#'   mutate(is_missing = label_missings(airquality,
#'                                      missing = "definitely missing",
#'                                      complete = "absolutely complete")) %>%
#'   head()
#' }
label_missings <- function(
  data,
  ...,
  missing = "Missing",
  complete = "Not Missing"
) {
  test_if_null(data)
  # find which are missing and which are not.

  any_row_na <- function(x) {
    apply(data.frame(x), MARGIN = 1, FUN = function(x) anyNA(x))
  }

  if (!missing(...)) {
    data <- dplyr::select(data, ...)
  }

  temp <- any_row_na(data)

  dplyr::if_else(
    condition = temp == TRUE, # TRUE means missing
    true = missing,
    false = complete
  )
}

#' Add a column describing if there are any missings in the dataset
#'
#' @param data data.frame
#' @param ... extra variable to label
#' @param missing character a label for when values are missing - defaults to "Missing"
#' @param complete character character a label for when values are complete - defaults to "Not Missing"
#'
#' @return data.frame with a column "any_missing" that is either "Not Missing"
#'   or "Missing" for the purposes of plotting / exploration / nice print methods
#' @export
#'
#' @seealso [bind_shadow()] [add_any_miss()] [add_label_missings()] [add_label_shadow()] [add_miss_cluster()] [add_n_miss()] [add_prop_miss()] [add_shadow_shift()] [cast_shadow()]
#'
#' @examples
#'
#' airquality %>% add_label_missings()
#' airquality %>% add_label_missings(Ozone, Solar.R)
#' airquality %>% add_label_missings(Ozone, Solar.R, missing = "yes", complete = "no")
#'
add_label_missings <- function(
  data,
  ...,
  missing = "Missing",
  complete = "Not Missing"
) {
  # data %>%
  #   dplyr::mutate(any_missing = label_missings(.)) %>%
  #   dplyr::as_tibble()

  updated_data <- data %>%
    dplyr::mutate(
      any_missing = label_missings(
        .,
        ...,
        missing = missing,
        complete = complete
      )
    )

  return(tibble::as_tibble(updated_data))
}

#' Label shadow values as missing or not missing
#'
#' Powers `add_label_shadow`. For the moment it is an internal function.
#'
#' @param data data.frame
#' @param ... extra variable to label
#' @param missing character a label for when values are missing - defaults to "Missing"
#' @param complete character character a label for when values are complete - defaults to "Not Missing"
#'
#' @return "Missing" or "Not Missing"
#' @keywords internal
#' @noRd
#'
label_shadow <- function(
  data,
  ...,
  missing = "Missing",
  complete = "Not Missing"
) {
  # any_shade <- function(x) any(grepl("^NA|^NA_", x))

  if (!missing(...)) {
    shadow_vars <- quo_to_shade(...)

    data <- dplyr::select(data, ..., !!!shadow_vars)
  }

  temp <- any_row_shade(data)
  dplyr::if_else(
    condition = temp == TRUE, # TRUE means missing
    true = missing,
    false = complete
  )
}

#' Add a column describing whether there is a shadow
#'
#' Instead of focussing on labelling whether there are missings, we instead
#'   focus on whether there have been any shadows created. This can be useful
#'   when data has been imputed and you need to determine which rows contained
#'   missing values when the shadow was bound to the dataset.
#'
#' @param data data.frame
#' @param ... extra variable to label
#' @param missing character a label for when values are missing - defaults to "Missing"
#' @param complete character character a label for when values are complete - defaults to "Not Missing"
#'
#' @return data.frame with a column, "any_missing", which describes whether or
#'   not there are any rows that have a shadow value.
#'
#' @export
#'
#' @seealso [bind_shadow()] [add_any_miss()] [add_label_missings()] [add_label_shadow()] [add_miss_cluster()] [add_n_miss()] [add_prop_miss()] [add_shadow_shift()] [cast_shadow()]
#'
#' @examples
#'
#' airquality %>%
#'   add_shadow(Ozone, Solar.R) %>%
#'   add_label_shadow()
#'
add_label_shadow <- function(
  data,
  ...,
  missing = "Missing",
  complete = "Not Missing"
) {
  if (!any_shade(data)) {
    rlang::abort(
      "add_label_shadow works with shadow data, which has columns
                 created by `shade()`, `as_shadow()`, or `bind_shadow()`"
    )
  }

  updated_data <- dplyr::mutate(
    data,
    any_missing = label_shadow(
      data,
      ...,
      missing = missing,
      complete = complete
    )
  )

  return(updated_data)
}


#' Add a column that tells us which "missingness cluster" a row belongs to
#'
#' A way to extract the cluster of missingness that a group belongs to.
#'     For example, if you use `vis_miss(airquality, cluster = TRUE)`, you can
#'     see some clustering in the data, but you do not have a way to identify
#'     the cluster. Future work will incorporate the `seriation` package to
#'     allow for better control over the clustering from the user.
#'
#' @param data a dataframe
#' @param cluster_method character vector of the agglomeration method to use,
#'    the default is "mcquitty". Options are taken from `stats::hclust`
#'    helpfile, and options include: "ward.D", "ward.D2", "single", "complete",
#'    "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or
#'    "centroid" (= UPGMC).
#' @param n_clusters numeric the number of clusters you expect. Defaults to 2.
#'
#' @seealso [bind_shadow()] [add_any_miss()] [add_label_missings()] [add_label_shadow()] [add_miss_cluster()] [add_n_miss()] [add_prop_miss()] [add_shadow_shift()] [cast_shadow()]
#'
#' @export
#'
#' @examples
#'
#' add_miss_cluster(airquality)
#' add_miss_cluster(airquality, n_clusters = 3)
#' add_miss_cluster(airquality, cluster_method = "ward.D", n_clusters = 3)

add_miss_cluster <- function(
  data,
  cluster_method = "mcquitty",
  n_clusters = 2
) {
  test_if_null(data)

  test_if_dataframe(data)

  data_na <- is.na(data)

  miss_cluster <- stats::dist(data_na * 1) %>%
    stats::hclust(method = cluster_method) %>%
    stats::cutree(k = n_clusters)

  data$miss_cluster <- miss_cluster
  tibble::as_tibble(data)
}
