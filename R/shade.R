# use new_shade

#' Create a new shade factor
#'
#' @param x a factor to convert into a `shade` object
#' @param extra_levels the extra levels to give to `shade` objects, such as "broken_machine" and so on, which get converted into "NA_broken_machine".
#'
#' @return a new shade, which is built upon a factor
new_shade <- function(x, extra_levels = NULL){

  if (!is.factor(x)) {
    rlang::abort(msg = "input to shade must be a factor")
  }

  levels(x) <- union(levels(x), glue::glue("NA_{extra_levels}"))
  structure(x,
            class = c("shade", "factor"))
}


#' Detect if this is a shade
#'
#' This tells us if this column is a shade
#'
#' @param x a vector you want to test if is a shade
#'
#' @return logical - is this a shade?
#' @export
#' @name is_shade
#'
#' @examples
#'
#' xs <- shade(c(NA, 1, 2, "3"))
#'
#' is_shade(xs)
#'
is_shade <- function(x){
  inherits(x, "shade")
}

#' @export
#' @rdname is_shade
are_shade <- function(x){
  purrr::map(x, class) %>%
    purrr::map_lgl(~any(grepl("shade",.)))
}

any_shade <- function(x){
  # any(grepl("_NA$",colnames(x)))
  any(are_shade(x))
}




#' Create new levels of missing
#'
#' Returns (at least) factors of !NA and NA, where !NA indicates a datum that is
#'   not missing, and NA indicates missingness. It also allows you to specify
#'   some new missings, if you like. This function is what powers the factor
#'   levels in `as_shadow()`.
#'
#' @param x a vector
#' @param ... additional levels of missing to add
#' @param extra_levels is a
#'
#' @examples
#' df <- tibble::tribble(
#'   ~wind, ~temp,
#'   -99,    45,
#'   68,    NA,
#'   72,    25
#'   )
#'
#' shade(df$wind)
#'
#' shade(df$wind,
#'       inst_fail = -99)
#'
#' shade(df$wind,
#'       inst_fail = 100)
#'
#' @export
shade <- function(x, ..., extra_levels = NULL){

  test_if_null(x)

  if (length(x) == 0) {
    rlang::abort(msg = "input to shade must have length > 0")
  }

  # if no other levels are specified
  if (missing(...)) {
    x <- factor(is.na(x),
                labels = c("!NA", "NA"),
                levels = c(FALSE, TRUE))

    return(new_shade(x, extra_levels))
  }

  # if additional levels are specified
  if (!missing(...)) {
  # capture the dots
  dict <- rlang::dots_list(...)

  # which values of x match the specified values
  match_pos <- match(x, dict)

  # the name of the new NA level
  custom_na_names <- paste0("NA_", names(dict))

  # add exception for when there is no matches anywhere
  # so skip this if there are no matches
  if (!all_na(match_pos)) {
    x[!is.na(match_pos)] <- custom_na_names[match_pos[!is.na(match_pos)]]
  }

  # add labels to those values that are missing
  # For those values that were not matched
    values_not_matched <- is.na(x[is.na(match_pos)])

  # find if they were missing
  x[is.na(match_pos)] <- ifelse(test = values_not_matched,
                                yes = "NA",
                                no = "!NA")


  x <- factor(x,
              levels = c("!NA", "NA", custom_na_names))
  }

  # and return a new shade value
  new_shade(x, extra_levels)
}
