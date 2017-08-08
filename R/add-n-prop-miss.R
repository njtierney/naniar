#' Add column containing number of missing data values
#'
#' It can be useful when doing data analysis to add the number of missing data
#'   points into your dataframe. `add_n_na` adds a column named "n_na",
#'   which contains the number of missing values in that row.
#'
#' @param data a dataframe
#' @param ... Variable names to use instead of the whole dataset. By default this
#'   looks at the whole dataset. Otherwise, this is one or more unquoted
#'   expressions separated by commas. These also respect the dplyr verbs
#'   `starts_with`, `contains`, `ends_with`, etc. By default will add "_all" to
#'   the label if left blank, otherwise will add "_vars" to distinguish that it
#'   has not been used on all of the variables.
#' @param label character default is "n_na".
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#'
#' airquality %>% add_n_na()
#' airquality %>% add_n_na(Ozone, Solar.R)
#' airquality %>% add_n_na(dplyr::contains("o"))
#'
#'
add_n_na <- function(data, ..., label = "n_na"){

  if (missing(...)) {
    purrrlyr::by_row(.d = data,
                     ..f = function(x) n_na(x),
                     .collate = "row",
                     .to = paste0(label,"_all"))
  } else {

    quo_vars <- rlang::quos(...)

    selected_data <- dplyr::select(data, !!!quo_vars)

    prop_selected_data <- purrrlyr::by_row(.d = selected_data,
                                           ..f = function(x) n_na(x),
                                           .collate = "row",
                                           .to =  paste0(label,"_vars"))

    # add only the variables prop_na function, not the whole data.frame...
    prop_selected_data_cut <- prop_selected_data %>%
      dplyr::select(!!as.name(paste0(label,"_vars")))

    dplyr::bind_cols(data, prop_selected_data_cut) %>% dplyr::as_tibble()

  } # close else loop

}

#' Add column containing proportion of missing data values
#'
#' It can be useful when doing data analysis to add the proportion of missing
#'   data values into your dataframe. `add_prop_na` adds a column named
#'   "prop_na", which contains the proportion of missing values in that row.
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
#' airquality %>% add_prop_na()
#'
#' airquality %>% add_prop_na(Solar.R)
#'
#' airquality %>% add_prop_na(Solar.R, Ozone)
#'
#' airquality %>% add_prop_na(Solar.R, Ozone, label = "testing")
#'
#' # this can be applied to model the proportion of missing data
#' # as in Tierney et al bmjopen.bmj.com/content/5/6/e007450.full
#' library(rpart)
#' library(rpart.plot)
#'
#' airquality %>%
#' add_prop_na() %>%
#' rpart(prop_na_all ~ ., data = .) %>%
#' prp(type = 4,
#'     extra = 101,
#'     prefix = "prop_na = ")

add_prop_na <- function(data, ..., label = "prop_na"){

  if (missing(...)) {
    purrrlyr::by_row(.d = data,
                     ..f = function(x) (mean(is.na(x))),
                     .collate = "row",
                     .to = paste0(label,"_all"))
  } else {

    quo_vars <- rlang::quos(...)

    selected_data <- dplyr::select(data, !!!quo_vars)

    prop_selected_data <- purrrlyr::by_row(.d = selected_data,
                                           ..f = function(x) prop_na(x),
                                           .collate = "row",
                                           .to =  paste0(label,"_vars"))

    # add only the variables prop_na function, not the whole data.frame...
    prop_selected_data_cut <- prop_selected_data %>%
      dplyr::select(!!as.name(paste0(label,"_vars")))

    dplyr::bind_cols(data, prop_selected_data_cut) %>% dplyr::as_tibble()

  }
}
