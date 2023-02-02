#' @name plotly_helpers
#' @title Plotly helpers (Convert a geom to a "basic" geom.)
#' @description Helper functions to make it easier to automatically create
#'   plotly charts. This function makes it possible to convert ggplot2 geoms
#'   that are not included with ggplot2 itself. Users shouldn't need to use this
#'   function. It exists purely to allow other package authors to write their
#'   own conversion method(s).
#' @param data the data returned by `ggplot2::ggplot_build()`.
#' @param prestats_data the data before statistics are computed.
#' @param layout the panel layout.
#' @param params parameters for the geom, statistic, and 'constant' aesthetics
#' @param p a ggplot2 object (the conversion may depend on scales, for
#'        instance).
#' @param ... currently ignored
#' @keywords internal
to_basic.GeomMissPoint <- function(data,
                                   prestats_data,
                                   layout,
                                   params,
                                   p,
                                   ...)  {
  prefix_class(data, "GeomPoint")
}

prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}
