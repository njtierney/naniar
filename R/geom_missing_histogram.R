#' geom_missing_histogram
#'
#' geom_missing_histogram adds a point geometry for displaying missingness.
#'
#' @note Very first attempt at creating a geom that is compatible with ggplot2.
#' Data plotting works. Still todo:
#' manipulate the colour aes so that the colours and legend appear.
#' fix awful default point sizes.
#' Warning message if na.rm = T is supplied.
#'
#' @param ? unknown parameters...
#'
#' @import ggplot2
#'
# this code messes up the documentation
# library(ggplot2)
# library(ggmissing)
#
#  ggplot(data = brfss,
#         aes(x = PHYSHLTH)) +
#  geom_missing_histogram()
#'
#' @export
GeomMissingHistogram <- ggproto("GeomMissingHistogram", GeomRect,
                            required_aes = "x",
                            setup_data = function(data, params){
                              #TODO: print warning if na.rm = T
                              data$x <- shadow_shift(data$x)
                              data$missing <- label_missing_1d(data$x)
                              data
                              # this part is from the geom-bar code
                              data$width <- data$width %||%
                                params$width %||% (resolution(data$x, FALSE) * 0.9)
                              transform(data,
                                        ymin = pmin(y, 0), ymax = pmax(y, 0),
                                        xmin = x - width / 2, xmax = x + width / 2, width = NULL
                              )
                            } ,
                            default_aes = aes(colour = ..missing..),
                            draw_key = draw_key_point,
                            handle_na = function(self, data, params) data,
                            draw_panel = function(self, data, panel_scales, coord, width = NULL) {
                              # Hack to ensure that width is detected as a parameter
                              ggproto_parent(GeomRect, self)$draw_panel(data, panel_scales, coord)
                              coords <- coord$transform(data, panel_scales)
                              # grid::pointsGrob(
                              #   coords$x,
                              #   pch = coords$shape,
                              #   gp = grid::gpar(
                              #     col = coords$colour,
                              #     fill = alpha(coords$fill, coords$alpha),
                              #     # Stroke is added around the outside of the point
                              #     fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                              #     lwd = coords$stroke * .stroke / 2
                              #   )
                              # )
                            }
)

#' @export
geom_missing_histogram <- function(mapping = NULL,
                               data = NULL,
                               # stat = "bin",
                               position = "stack",
                               colour = ..missing..,
                               binwidth = NULL,
                               bins = NULL,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatMissingHistogram,
    geom = GeomMissingHistogram,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      pad = FALSE,
      ...
    )
  )

}
