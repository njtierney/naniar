the_mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) <= 1) {
    x
  }

  if (length(x) > 1) {
    d <- stats::density(x)
    d$x[which.max(d$y)]
  }
}

#' Impute the mode value into a vector with missing values
#'
#' @param x vector
#'
#' This approach adapts examples provided [from stack overflow](https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode), and for the integer
#'   case, just rounds the value. While this can be useful if you are
#'   imputing specific values, however we would generally recommend to impute
#'   using other model based approaches. See the `simputation` package, for
#'   example [simputation::impute_lm()].
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
#' library(dplyr)
#'
#' dat <- tibble(
#'   num = rnorm(10),
#'   int = rpois(10, 5),
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
#'
#' dat %>%
#'   nabular() %>%
#'   mutate(
#'     num = impute_mode(num),
#'     int = impute_mode(int),
#'     fct = impute_mode(fct)
#'   )
#'
#'
impute_mode <- function(x) UseMethod("impute_mode")

#' @export
#' @rdname impute_mode
impute_mode.default <- function(x) {
  x[is.na(x)] <- the_mode(x, na.rm = TRUE)

  x
}

#' @export
#' @rdname impute_mode
impute_mode.integer <- function(x) {
  x[is.na(x)] <- round(the_mode(x, na.rm = TRUE))

  x
}

#' @export
#' @rdname impute_mode
impute_mode.factor <- function(x) {
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
