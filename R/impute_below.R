#' Impute data with values shifted 10\% below range.
#'
#' It can be useful in exploratory graphics to impute data outside the range of
#'   the data. `impute_below` imputes values 10% below the range for numeric
#'   values, and for character or factor values adds a new string or label.
#'
#' @param .tbl a data.frame
#' @param ... additional arguments
#'
#' @return an dataset with values imputed
#' @export
#'
#' @examples
#'
#' # you can impute data like so:
#' airquality %>%
#'   impute_below()
#'
#' # However, this does not show you WHERE the missing values are.
#' # to keep track of them, you want to use `bind_shadow()` first.
#'
#' airquality %>%
#'   bind_shadow() %>%
#'   impute_below()
#'
#' # This identifies where the missing values are located, which means you
#' # can do things like this:
#'
#' library(ggplot2)
#' airquality %>%
#'   bind_shadow() %>%
#'   impute_below() %>%
#'   # identify where there are missings across rows.
#'   add_label_shadow() %>%
#'   ggplot(aes(x = Ozone,
#'              y = Solar.R,
#'              colour = any_missing)) +
#'   geom_point()
#'
#' # This is a long version of `geom_miss_point()`.
#'
impute_below <- function(.tbl, ...){

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_all(.tbl = .tbl,
                    .funs = shadow_shift)

}

#' Scoped variants of `impute_below`
#'
#' `impute_below` operates on all variables. To only impute variables
#'   that satisfy a specific condition, use the scoped variants,
#'   `impute_below_at`, and `impute_below_if`.
#'
#' @param .tbl a data.frame
#' @param .vars variables to impute
#' @param ... extra arguments
#'
#' @return an dataset with values imputed
#' @export
#'
#' @examples
#' # select variables starting with a particular string.
#' library(dplyr)
#' impute_below_at(airquality,
#'                 .vars = starts_with("Oz"))
#'
#' impute_below_at(airquality,
#'                 .vars = 1:2)
#'
#' impute_below_at(airquality,
#'                 .vars = everything())
#'
#'
impute_below_at <- function(.tbl, .vars, ...){

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  .vars <- tidyselect::vars_select(names(.tbl), .vars)

  dplyr::mutate_at(.tbl = .tbl,
                   .vars = .vars,
                   .funs = shadow_shift)
}

#' Scoped variants of `impute_below`
#'
#' `impute_below` operates on all variables. To only impute variables
#'   that satisfy a specific condition, use the scoped variants,
#'   `impute_below_at`, and `impute_below_if`.
#'
#' @param .tbl data.frame
#' @param .predicate A predicate function (such as is.numeric)
#' @param ... extra arguments
#'
#' @return an dataset with values imputed
#' @export
#' @examples
#'
#' airquality %>%
#'   impute_below_if(.predicate = is.numeric)
#'
impute_below_if <- function(.tbl, .predicate, ...){

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_if(.tbl = .tbl,
                   .predicate = .predicate,
                   .funs = shadow_shift)
}
