#' Plot Missing Data Points
#'
#' `geom_na_point` provides a way to transform and plot missing values in
#'   ggplot2. To do so it uses methods from ggobi to display missing data
#'   points 10% below the minimum value, so that the values can be seen no the
#'   same axis.
#'
#' @include legend-draw.R
#' @name  geom_na_point
#' @title  geom_na_point
#' @note Warning message if na.rm = T is supplied.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#' or [ggplot2::aes_()]. If specified and `inherit.aes = TRUE`
#' (the default), is combined with the default mapping at the top level of the
#' plot. You only need to supply mapping if there isn't a mapping defined for
#' the plot.
#'
#' @param data A data frame. If specified, overrides the default data frame
#' defined at the top level of the plot.
#'
# @param stat The statistical transformation to use on the data for this layer, as a string.
#'
#' @param position Position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#'
#' @param na.rm If `FALSE` (the default), removes missing values with a
#' warning. If `TRUE` silently removes missing values.
#'
#' @param show.legend logical. Should this layer be included in the legends?
#' `NA`, the default, includes if any aesthetics are mapped. `FALSE`
#' never includes, and `TRUE` always includes.
#'
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather
#' than combining with them. This is most useful for helper functions that
#' define both data and aesthetics and shouldn't inherit behaviour from the
#' default plot specification, e.g. borders.
#'
#' @param ... other arguments passed on to [ggplot2::layer()]. There
#' are three types of arguments you can use here:
#' \itemize{
#'  \item{Aesthetics: to set an aesthetic to a fixed value, like
#'  `color = "red"` or `size = 3.`}
#'  \item{Other arguments to the layer, for example you override the default
#'  `stat` associated with the layer.}
#'  \item{Other arguments passed on to the stat.}
#' }
#'
#' @param colour the colour chosen for the aesthetic
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # using regular geom_point()
#' ggplot(airquality,
#'        aes(x = Ozone,
#'            y = Solar.R)) +
#' geom_point()
#'
#' # using  geom_na_point()
#' ggplot(airquality,
#'        aes(x = Ozone,
#'            y = Solar.R)) +
#'  geom_na_point()
#'
#' @export
 geom_na_point <- function(mapping = NULL,
                               data = NULL,
                               # stat = "identity",
                               position = "identity",
                               colour = ..missing..,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatNaPoint,
    geom = GeomNaPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )

}

#' @rdname naniar-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomNaPoint <- ggproto("GeomNaPoint", GeomPoint,
                            required_aes = c("x", "y"),
                            default_aes = aes(shape = 19,
                                              colour = ..missing..,
                                              size = 0.5,
                                              fill = NA,
                                              alpha = NA,
                                              stroke = 1.5),
                            draw_key = draw_key_missing_point,
                            setup_data = function(data, params){
                              #TODO: print warning if na.rm = T
                              data$x <- shadow_shift(data$x)
                              data$y <- shadow_shift(data$y)
                              data$missing <- label_na_2d(data$x, data$y)
                              data
                            } ,
                            handle_na = function(self, data, params) data,
                            draw_panel = function(data, panel_scales, coord) {
                              coords <- coord$transform(data, panel_scales)
                              grid::pointsGrob(
                                coords$x, coords$y,
                                pch = coords$shape,
                                gp = grid::gpar(
                                  col = alpha(coords$colour, coords$alpha),
                                  fill = alpha(coords$fill, coords$alpha),
                                  # Stroke is added around the outside of the point
                                  fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                  lwd = coords$stroke * .stroke / 2
                                )
                              )
                            }
)

