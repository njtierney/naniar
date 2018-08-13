#' Create a new nabular format
#'
#' @param x a data.frame
#'
#' @return object with class "nabular", inhereting from it's original class
#' @export
new_nabular <- function(x){

  # include tests for checking that the data contains a shadow

  tibble::new_tibble(x, subclass = "nabular")
}
