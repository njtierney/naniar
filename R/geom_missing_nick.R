# #' geom_missing_nick
# #'
# #' geom_missing adds a geometry for displaying missingness.
# #'
# #' @note At this stage geom_missing is limited to geom_point
# #'
# #' @note Very first attempt at creating a geom that is compatible with ggplot2.
# #' Data plotting works. Still todo:
# #' manipulate the colour aes so that the colours and legend appear.
# #' fix awful default point sizes.
# #' Warning message if na.rm = T is supplied.
# #'
# #' @param ? unknown parameters...
# #'
# #' @import ggplot2
# #'
# # this code messes up the documentation
# # library(ggplot2)
# # library(ggmissing)
# #
# #  ggplot(data = brfss,
# #         aes(x = PHYSHLTH,
# #             y = POORHLTH) ) +
# #  geom_point_missing()
#
# #' @export
# fill_missing <- function(x1, x2){
#
#   # find which are missing and which are not.
#   temp <- data.frame(x1,x2) %>% is.na %>% rowSums()
#   ifelse(temp == 0,
#          "Not Missing",
#          "Missing")
#
# }
#
# #' @export
# geom_missing_nick <- function(mapping = NULL,
#                               data = NULL, ...,
#                               missing.colour = NULL,
#                               na.rm = FALSE,
#                               show.legend = NA,
#                               inherit.aes = TRUE) {
#   layer(
#     data = data,
#     mapping = mapping,
#     stat = "identity",
#     geom = GeomMissingNick,
#     position = "identity",
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       na.rm = na.rm,
#       missing.colour = missing.colour,
#       ...
#     )
#   )
#
# }
# #' @rdname ggmissing-ggproto
# #' @format NULL
# #' @usage NULL
# #' @export
# GeomMissingNick <- ggproto("GeomMissingNick", Geom,
#     required_aes = c("x", "y"),
#
#     non_missing_aes = c("size",
#                         "shape",
#                         "missing.colour"),
#
#     default_aes = aes(
#       shape = 19,
#       colour = NULL,
#       size = 0.5,
#       fill = NA,
#       alpha = NA,
#       stroke = 1.5),
#
#     setup_data = function(data, params){
#       #TODO: print warning if na.rm = T
#       # be the fill colour
#       data$missing.colour <- fill_missing(data$x, data$y)
#       data$x <- shadow_shift(data$x)
#       data$y <- shadow_shift(data$y)
#       } ,
#     handle_na = function(self, data, params) data,
#
#     draw_panel = function(data, panel_scales, coord) {
#                              coords <- coord$transform(data, panel_scales)
#                              grid::pointsGrob(
#                                coords$x, coords$y,
#                                pch = coords$shape,
#                                gp = grid::gpar(col = coords$colour)
#                              )
#                              },
#     draw_key = draw_key_point
# )
#
#
