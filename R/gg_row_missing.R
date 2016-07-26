#' gg_row_missing
#'
#' This function draws a ggplot plot of the number of missings in each row.
#' At this point I'm not sure how this plot should be arranged, as it currently looks ugly!
#'
#' @param x a dataframe
#'
#' @return a ggplot plot depicting the number of missings in a given row
#' @export
#'
#' @examples
#'
#' gg_row_missings(airquality)
#'
gg_row_missing <- function(x){
# x <- airquality
  # get a tidy data frame of the number of missings in each column
  x_row_missing <- row_missing(x)
  #
  # ggplot(data = x_row_missing,
  #        aes(x = factor(n_col),
  #            y = row)) +
  #   geom_raster(aes(fill = factor(n_missing))) +
  #   labs(x = "",
  #        y = "row #") +
  #   theme_minimal()

  ggplot(data = x_row_missing,
         aes(x = factor(n_missing),
             y = reorder(row, n_missing))) +
    geom_point() +
    labs(x = "# Missing",
         y = "Variables") +
    theme_minimal()

}
