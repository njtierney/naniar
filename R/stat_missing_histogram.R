#' stat_missing_histogram
#'
#' stat_missing_histogram adds a geometry for displaying missingness to
#' geom_histogram
#'
#' @note Very first attempt at creating a geom that is compatible with ggplot2.
#' Data plotting works. Still todo:
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
# ggplot(data = airquality,
#        aes(x = Ozone) +
#   geom_histogram()
#
# ggplot(data = airquality,
#        aes(x = Ozone)) +
#   geom_missing_histogram()
#

#' @export
#' @rdname ggmissing-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatMissingHistogram <- ggproto("StatMissingHistogram", Stat,
                            required_aes = c("x"),
                            default_aes = aes(colour = ..missing..),
                            setup_data = function(data, params){
                              #TODO: print warning if na.rm = T
                              data$x_miss <- data$x
                              data$x <- shadow_shift(data$x)
                              data
                            } ,

                            handle_na = function(self, data, params) data,
                            compute_group = function(data, scales) {
                              missing_label <- label_missing_1d(data$x_miss)

                              data.frame(x = data$x,
                                         missing = missing_label)

                            }
)

#' @export
stat_missing_histogram <- function(mapping = NULL,
                               data = NULL,
                               geom = "histogram",
                               position = "identity",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               ...) {
  ggplot2::layer(
    stat = StatMissingHistogram,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}
