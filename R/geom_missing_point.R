#' geom_missing_point
#'
#' geom_missing_point adds a point geometry for displaying missingness.
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
#         aes(x = PHYSHLTH,
#             y = POORHLTH) ) +
#  geom_missing()
GeomMissingPoint <- ggproto("GeomMissingPoint",
                       Geom,
                       required_aes = c("x", "y"),
                       default_aes = aes(shape = 19,
                                         colour = "black",
                                         size = 0.5,
                                         fill = NA,
                                         alpha = NA,
                                         stroke = 1.5),
                       draw_key = draw_key_point,
                       setup_data = function(data, params){
                         #TODO: print warning if na.rm = T
                         data$x <- shadow_shift(data$x)
                         data$y <- shadow_shift(data$y)
                         data
                       } ,

                       handle_na = function(self, data, params) data,

                       draw_panel = function(data, panel_scales, coord) {
                         coords <- coord$transform(data, panel_scales)
                         grid::pointsGrob(
                           coords$x, coords$y,
                           pch = coords$shape,
                           gp = grid::gpar(col = coords$colour)
                         )
                       }
)

#' @export
geom_missing_point <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMissingPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )

}
