#' Impute the median value into a vector with missing values
#'
#' @param x vector
#'
#' @return vector with median values replaced
#' @export
#' @name impute_median
#'
#' @examples
#'
#' vec <- rnorm(10)
#'
#' vec[sample(1:10, 3)] <- NA
#'
#' impute_median(vec)
#'
impute_median <- function(x) UseMethod("impute_median")

#' @export
#' @rdname impute_median
impute_median.default <- function(x){

  x[is.na(x)] <- median(x, na.rm = TRUE)

  x
}

#' @export
#' @rdname impute_median
impute_median.factor <- function(x){

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

#' Scoped variants of `impute_median`
#'
#' `impute_median` imputes the median for a vector. To only impute many
#'   variables at once, we recommend that you use the  `across` function
#'   workflow, shown in the examples for [impute_median()]. You can use the
#'   scoped variants, `impute_median_all`.`impute_below_at`, and
#'   `impute_below_if` to impute all, some, or just those variables meeting
#'   some condition, respectively. To use `_at` effectively, you must know
#'   that `_at` affects variables selected with a character vector, or with
#'   `vars()`.
#'
#' `r lifecycle::badge('superseded')`
#'
#' @param .tbl a data.frame
#' @param .vars variables to impute
#' @param .predicate variables to impute
#' @name scoped-impute_median
#'
#' @return an dataset with values imputed
#' @export
#'
#' @examples
#' # select variables starting with a particular string.
#' impute_median_all(airquality)
#'
#' impute_median_at(airquality,
#'                .vars = c("Ozone", "Solar.R"))
#' \dontrun{
#' library(dplyr)
#' impute_median_at(airquality,
#'                 .vars = vars(Ozone))
#'
#' impute_median_if(airquality,
#'                 .predicate = is.numeric)
#'
#' library(ggplot2)
#' airquality %>%
#'   bind_shadow() %>%
#'   impute_median_all() %>%
#'   add_label_shadow() %>%
#'   ggplot(aes(x = Ozone,
#'              y = Solar.R,
#'              colour = any_missing)) +
#'          geom_point()
#' }
#'
impute_median_all <- function(.tbl){

  signal_stage("superceded", "impute_median_all()")

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_all(.tbl = .tbl,
                    .funs = impute_median)

}

#' @export
#' @rdname scoped-impute_median
impute_median_at <- function(.tbl,
                           .vars){

  signal_stage("superceded", "impute_median_at()")

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_at(.tbl = .tbl,
                   .vars = .vars,
                   .funs = impute_median)

}

#' @export
#' @rdname scoped-impute_median
impute_median_if <- function(.tbl,
                           .predicate){

  signal_stage("superceded", "impute_median_if()")

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_if(.tbl = .tbl,
                   .predicate = .predicate,
                   .funs = impute_median)

}
