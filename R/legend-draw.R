#' Key drawing functions
#'
#' Each Geom has an associated function that draws the key when the geom needs
#'   to be displayed in a legend. These are the options built into naniar.
#'
#' @return A grid grob.
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @keywords internal
#' @name draw_key
NULL

#' @export
#' @rdname draw_key
draw_key_missing_point <- function(data, params, size) {
  grid::pointsGrob(
    0.5,
    0.5,
    pch = data$shape,
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      fontsize = data$size * .pt + data$stroke * .stroke / 2,
      lwd = data$stroke * .stroke / 2
    )
  )
}
