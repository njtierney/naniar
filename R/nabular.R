#' Create a new nabular format
#'
#' @param x a data.frame
#'
#' @return object with class "nabular", inheriting from it's original class
#' @export
new_nabular <- function(x){

  # is there some mixture of shade and regular data?
  if (sum(are_shade(x) == ncol(x)) | !any_shade(x)) {
    rlang::abort(message = "data must have shadow data with the regular data")
  }
  tibble::new_tibble(x, class = "nabular", nrow = as.integer(nrow(x)))
}

#' Convert data into nabular form by binding shade to it
#'
#' Binding a shadow matrix to a regular dataframe converts it into nabular data,
#'   which makes it easier to visualise and work with missing data.
#'
#' @param data a dataframe
#' @param only_miss logical - if FALSE (default) it will bind a dataframe with
#'     all of the variables duplicated with their shadow. Setting this to TRUE
#'     will bind variables only those variables that contain missing values.
#'     See the examples for more details.
#' @param ... extra options to pass to [recode_shadow()] - a work in progress.
#'
#' @return data with the added variable shifted and the suffix `_NA`
#' @export
#' @seealso [bind_shadow()]
#'
#' @examples
#'
#' aq_nab <- nabular(airquality)
#' aq_s <- bind_shadow(airquality)
#'
#' all.equal(aq_nab, aq_s)
#'
#' @export
nabular <- function(data, only_miss = FALSE, ...){
  bind_shadow(data = data, only_miss = only_miss, ...)
}
