#' @name stat_missing_point
#' @title stat_missing_point
#' @description stat_missing_point adds a geometry for displaying missingness to geom_point
#'
#' @note Very first attempt at creating a geom that is compatible with ggplot2.
#' Data plotting works. Still todo:
#' Warning message if na.rm = T is supplied.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' or \code{\link[ggplot2]{aes_}}. If specified and \code{inherit.aes = TRUE}
#' (the default), is combined with the default mapping at the top level of the
#' plot. You only need to supply mapping if there isn't a mapping defined for
#' the plot.
#'
#' @param data A data frame. If specified, overrides the default data frame
#' defined at the top level of the plot.
#'
#' @param geom, stat Override the default connection between \code{geom_point} and \code{stat_point}.
#'
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function
#'
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#' warning. If \code{TRUE} silently removes missing values.
#'
#' @param show.legend logical. Should this layer be included in the legends?
#' \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#' never includes, and \code{TRUE} always includes.
#'
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#' than combining with them. This is most useful for helper functions that
#' define both data and aesthetics and shouldn't inherit behaviour from the
#' default plot specification, e.g. borders.
#'
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There
#' are three types of arguments you can use here:
#' \itemize{
#'  \item{Aesthetics: to set an aesthetic to a fixed value, like
#'  \code{color = "red"} or \code{size = 3.}}
#'  \item{Other arguments to the layer, for example you override the default
#'  \code{stat} associated with the layer.}
#'  \item{Other arguments passed on to the stat.}
#' }
#'
#' @export
stat_missing_point <- function(mapping = NULL,
                               data = NULL,
                               geom = "point",
                               position = "identity",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               ...) {
  ggplot2::layer(
    stat = StatMissingPoint,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

#' @rdname narnia-ggproto
#' @export
StatMissingPoint <- ggproto("StatMissingPoint", Stat,
    required_aes = c("x", "y"),
    default_aes = aes(colour = ..missing..),
    setup_data = function(data, params){
      #TODO: print warning if na.rm = T
      data$x_miss <- data$x
      data$y_miss <- data$y
      data$x <- shadow_shift(data$x)
      data$y <- shadow_shift(data$y)
      data
      } ,

    handle_na = function(self, data, params) data,
    compute_group = function(data, scales) {
      missing_label <- label_missing_2d(data$x_miss, data$y_miss)

      data.frame(x = data$x,
                 y = data$y,
                 missing = missing_label)

      }
    )


