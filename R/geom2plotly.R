#' @name plotly_helpers
#' @title Plotly helpers
#' @description Helper functions to make it easier to automatically create plotly charts
#' @export
to_basic.GeomMissPoint <- function(data, prestats_data, layout, params, p, ...)  {
  View(data)
  prefix_class(data, "GeomPoint")
}

prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}
