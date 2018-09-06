#' Impute the mode value into a vector with missing values
#'
#' @param x vector
#'
#' @return vector with mode values replaced
#' @export
#' @name impute_mode
#'
#' @examples
#'
#' vec <- rnorm(10)
#'
#' vec[sample(1:10, 3)] <- NA
#'
#' impute_mode(vec)
#'
impute_mode <- function(x) UseMethod("impute_mode")

#' @export
#' @rdname impute_mode
impute_mode.default <- function(x){

  x[is.na(x)] <- the_mode(x, na.rm = TRUE)

  x
}

#' @export
#' @rdname impute_mode
impute_mode.factor <- function(x){

  i_mode <- function(x){

    tab <- table(x)
    max_tab <- max(tab)
    if (all(tab == max_tab)) {mod = NA}

    if (is.numeric(x)) {
      mod <- as.numeric(names(tab)[tab == max_tab])
    }

    mod <- names(tab)[tab == max_tab]

    # randomly break a tie
    return(sample(mod, 1))
  }

  x[is.na(x)] <- i_mode(x)

  x
}

#' Scoped variants of `impute_mode`
#'
#' `impute_mode` imputes the mode for a vector. To get it to work on all
#'   variables, use `impute_mode_all`. To only impute variables
#'   that satisfy a specific condition, use the scoped variants,
#'   `impute_below_at`, and `impute_below_if`. To use `_at` effectively,
#'   you must know that `_at`` affects variables selected with a character
#'   vector, or with `vars()`.
#'
#' @param .tbl a data.frame
#' @param .vars variables to impute
#' @param .predicate variables to impute
#' @name scoped-impute_mode
#'
#' @return an dataset with values imputed
#' @export
#'
#' @examples
#' # select variables starting with a particular string.
#' library(dplyr)
#' impute_mode_all(airquality)
#'
#' impute_mode_at(airquality,
#'                .vars = c("Ozone", "Solar.R"))
#'
#' impute_mode_at(airquality,
#'                 .vars = vars(Ozone))
#'
#' impute_mode_if(airquality,
#'                 .predicate = is.numeric)
#'
#' \dontrun{
#' library(ggplot2)
#' airquality %>%
#'   bind_shadow() %>%
#'   impute_mode_all() %>%
#'   add_label_shadow() %>%
#'   ggplot(aes(x = Ozone,
#'              y = Solar.R,
#'              colour = any_missing)) +
#'          geom_point()
#' }
#'
impute_mode_all <- function(.tbl){

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_all(.tbl = .tbl,
                    .funs = impute_mode)

}

#' @export
#' @rdname scoped-impute_mode
impute_mode_at <- function(.tbl,
                             .vars){

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_at(.tbl = .tbl,
                   .vars = .vars,
                   .funs = impute_mode)

}

#' @export
#' @rdname scoped-impute_mode
impute_mode_if <- function(.tbl,
                             .predicate){

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_if(.tbl = .tbl,
                   .predicate = .predicate,
                   .funs = impute_mode)

}
