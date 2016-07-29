#' stat_missing
#'
#' stat_missing adds a geometry for displaying missingness.
#'
#' @note At this stage stat_missing is limited to geom_point
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
#
# ggplot(data = airquality,
#        aes(x = Ozone,
#            y = Solar.R)) +
#   stat_missing()
#
# ggplot(data = airquality,
#        aes(x = Ozone,
#            y = Solar.R)) +
#   geom_missing_point()
#
# ggplot(data = airquality,
#        aes(x = Ozone,
#            y = Solar.R)) +
#   geom_point()

#' @export
# fill_missing <- function(x1, x2){
#
#   # find which are missing and which are not.
#   temp <- data.frame(x1,x2) %>% is.na %>% rowSums()
#   ifelse(temp == 0,
#          "Not Missing",
#          "Missing")
#
# }
#' @rdname ggmissing-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatMissing <- ggproto("StatMissing", Stat,
                       required_aes = c("x", "y"),
                       default_aes = aes(colour = ..missing..),
                       setup_data = function(data, params){
                         #TODO: print warning if na.rm = T
                         data$x <- shadow_shift(data$x)
                         data$y <- shadow_shift(data$y)
                         # data$..missing.. <- label_missing_2d(data$x, data$y)
                         data
                       } ,

                       handle_na = function(self, data, params) data,
                       compute_group = function(data, scales) {

                         missing_label <- label_missing_2d(data$x, data$y)

                         data.frame(x = shadow_shift(data$x),
                                    y = shadow_shift(data$y),
                                    missing = missing_label)

                       }
                       )

#' @export
stat_missing <- function(mapping = NULL,
                         data = NULL,
                         geom = "point",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  ggplot2::layer(
    stat = StatMissing,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}
