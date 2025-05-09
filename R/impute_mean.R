#' Impute the mean value into a vector with missing values
#'
#' This can be useful if you are imputing specific values, however we would
#'   generally recommend to impute using other model based approaches. See
#'   the `simputation` package, for example [simputation::impute_lm()].
#'
#'
#' @param x vector
#'
#' @return vector with mean values replaced
#' @export
#' @name impute_mean
#'
#' @examples
#'
#' library(dplyr)
#' vec <- rnorm(10)
#'
#' vec[sample(1:10, 3)] <- NA
#'
#' impute_mean(vec)
#'
#' dat <- tibble(
#'   num = rnorm(10),
#'   int = as.integer(rpois(10, 5)),
#'   fct = factor(LETTERS[1:10])
#' ) %>%
#'   mutate(
#'     across(
#'       everything(),
#'       \(x) set_prop_miss(x, prop = 0.25)
#'     )
#'   )
#'
#' dat
#'
#' dat %>%
#'   nabular() %>%
#'   mutate(
#'     num = impute_mean(num),
#'     int = impute_mean(int),
#'     fct = impute_mean(fct),
#'   )
#'
#' dat %>%
#'   nabular() %>%
#'   mutate(
#'     across(
#'       where(is.numeric),
#'       impute_mean
#'     )
#'   )
#'
#' dat %>%
#'   nabular() %>%
#'   mutate(
#'     across(
#'       c("num", "int"),
#'       impute_mean
#'     )
#'   )
#'
impute_mean <- function(x) UseMethod("impute_mean")

#' @export
#' @rdname impute_mean
impute_mean.default <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)

  x
}

#' @export
#' @rdname impute_mean
impute_mean.factor <- function(x) {
  i_mode <- function(x) {
    tab <- table(x)
    max_tab <- max(tab)
    if (all(tab == max_tab)) {
      mod = NA
    }

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

#' Scoped variants of `impute_mean`
#'
#' `impute_mean` imputes the mean for a vector. To get it to work on all
#'   variables, use `impute_mean_all`. To only impute variables
#'   that satisfy a specific condition, use the scoped variants,
#'   `impute_below_at`, and `impute_below_if`. To use `_at` effectively,
#'   you must know that `_at`` affects variables selected with a character
#'   vector, or with `vars()`.
#'
#' `r lifecycle::badge('superseded')`
#'
#' @param .tbl a data.frame
#' @param .vars variables to impute
#' @param .predicate variables to impute
#' @name scoped-impute_mean
#'
#' @return an dataset with values imputed
#' @export
#'
#' @examples
#' # select variables starting with a particular string.
#' impute_mean_all(airquality)
#'
#' impute_mean_at(airquality,
#'                .vars = c("Ozone", "Solar.R"))
#'
#' \dontrun{
#' library(dplyr)
#' impute_mean_at(airquality,
#'                 .vars = vars(Ozone))
#'
#' impute_mean_if(airquality,
#'                 .predicate = is.numeric)
#'
#' library(ggplot2)
#' airquality %>%
#'   bind_shadow() %>%
#'   impute_mean_all() %>%
#'   add_label_shadow() %>%
#'   ggplot(aes(x = Ozone,
#'              y = Solar.R,
#'              colour = any_missing)) +
#'          geom_point()
#' }
#'
impute_mean_all <- function(.tbl) {
  lifecycle::signal_stage("superseded", "impute_below_all()")

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_all(.tbl = .tbl, .funs = impute_mean)
}

#' @export
#' @rdname scoped-impute_mean
impute_mean_at <- function(.tbl, .vars) {
  lifecycle::signal_stage("superseded", "impute_below_all()")

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_at(.tbl = .tbl, .vars = .vars, .funs = impute_mean)
}

#' @export
#' @rdname scoped-impute_mean
impute_mean_if <- function(.tbl, .predicate) {
  lifecycle::signal_stage("superseded", "impute_below_all()")

  test_if_dataframe(.tbl)

  test_if_null(.tbl)

  dplyr::mutate_if(.tbl = .tbl, .predicate = .predicate, .funs = impute_mean)
}
