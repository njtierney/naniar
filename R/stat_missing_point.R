#' @name stat_missing_point
#' @title stat_missing_point
#' @description stat_missing_point adds a geometry for displaying missingness to geom_point
#'
#' @note Very first attempt at creating a geom that is compatible with ggplot2.
#' Data plotting works. Still todo:
#' Warning message if na.rm = T is supplied.
#'
#' @param ? unknown parameters...
#'
#' @import ggplot2
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

#' @rdname ggmissing-ggproto
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


