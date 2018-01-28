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
#'   `starts_with`, `contains`, `ends_with`, etc. By default will add "_all" to
#'   the label if left blank, otherwise will add "_vars" to distinguish that it
#'   has not been used on all of the variables.
#' @param label character default is "n_miss".
#'
#' @return a dataframe
#'
#' @export
#'
#' @seealso [bind_shadow()] [add_any_miss()] [add_label_missings()] [add_label_shadow()] [add_miss_cluster()] [add_prop_miss()] [add_shadow_shift()] [cast_shadow()]
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
    data[[paste0(label, "_all")]] <- n_miss_row(data)
  } else {

    quo_vars <- rlang::quos(...)

    selected_data <- dplyr::select(data, !!!quo_vars)

    data[[paste0(label, "_vars")]] <- n_miss_row(selected_data)
  } # close else loop

  data
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
#' @seealso [bind_shadow()] [add_any_miss()] [add_label_missings()] [add_label_shadow()] [add_miss_cluster()] [add_prop_miss()] [add_shadow_shift()] [cast_shadow()]
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
    data[[paste0(label, "_all")]] <- prop_miss_row(data)
  } else {

    quo_vars <- rlang::quos(...)

    selected_data <- dplyr::select(data, !!!quo_vars)

    data[[paste0(label, "_vars")]] <- prop_miss_row(selected_data)

  } # close else loop

  data
}
