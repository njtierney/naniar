#' @name stat_miss_point
#' @title stat_miss_point
#' @description stat_miss_point adds a geometry for displaying missingness to
#'   geom_point
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]
#'   or [ggplot2::aes_()]. If specified and `inherit.aes = TRUE`
#'   (the default), is combined with the default mapping at the top level of the
#'   plot. You only need to supply mapping if there isn't a mapping defined for
#'   the plot.
#'
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#'
#' @param prop_below the degree to shift the values. The default is 0.1
#'
#' @param jitter the amount of jitter to add. The default is 0.05
#'
#' @param geom, stat Override the default connection between `geom_point` and
#'   `stat_point`.
#'
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function
#'
#' @param na.rm If `FALSE` (the default), removes missing values with a warning.
#'   If `TRUE` silently removes missing values.
#'
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped. `FALSE`
#'   never includes, and `TRUE` always includes.
#'
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. borders.
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
#' @export
stat_miss_point <- function(mapping = NULL,
                            data = NULL,
                            prop_below = 0.1,
                            jitter = 0.05,
                            geom = "point",
                            position = "identity",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            ...) {
  ggplot2::layer(
    stat = StatMissPoint,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(prop_below = prop_below,
                  jitter = jitter,
                  na.rm = na.rm, ...)
  )

}

#' @rdname naniar-ggproto
#' @export
StatMissPoint <- ggproto("StatMissPoint", Stat,
    required_aes = c("x", "y"),
    default_aes = aes(colour = ..missing..),
    setup_data = function(data, params){
      #TODO: print warning if na.rm = T
      data$x_miss <- data$x
      data$y_miss <- data$y
      data$x <- shadow_shift(data$x,
                             prop_below = params$prop_below,
                             jitter = params$jitter)

      data$y <- shadow_shift(data$y,
                             prop_below = params$prop_below,
                             jitter = params$jitter)
      data
      } ,

    handle_na = function(self, data, params) data,
    compute_group = function(data, scales, prop_below = 0.1, jitter = 0.05) {
      missing_label <- label_miss_2d(data$x_miss, data$y_miss)

      data.frame(data,
                 missing = missing_label)

      }
    )


