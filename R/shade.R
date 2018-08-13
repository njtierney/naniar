# use new_shade
#' @export
new_shade <- function(x, extra_levels = NULL){

  if (!is.factor(x)) {
    rlang::abort(message = "input to shade must be a factor")
  }

  levels(x) <- union(levels(x), glue::glue("NA_{extra_levels}"))
  structure(x,
            class = c("shade", "factor"))
}


#' Create new levels of missing
#'
#' @param x a vector
#'
#' @param ... additional levels of missing to add
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
  # if no other levels are specified
  if (missing(...)) {
    x <- factor(is.na(x),
                labels = c("NA", "!NA"),
                levels = c(TRUE, FALSE))

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
              levels = c("NA", "!NA", custom_na_names))
  }

  # and return a new shade value
  new_shade(x, extra_levels)
}
