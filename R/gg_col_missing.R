#' gg_col_missings
#'
#' This function draws a ggplot plot of the number of missings in each column, rearranged to show which variables have the most missing data. At this point I'm still debating whether or not this should be with `geom_bar` or with `geom_lollipop` from hrbrmstr's `ggalt` package.
#'
#' @param x a dataframe
#'
#' @return a ggplot plot depicting the number of missings in a given column
#' @export
#'
#' @examples
#'
#' gg_col_missings(airquality)
#'
gg_col_missing <- function(x){

  # get a tidy data frame of the number of missings in each column
  x %>%
    col_missing() %>%
    ggplot(data = .,
           aes(x = n_missing,
               y = reorder(variable, n_missing),
               colour = variable)) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    labs(x = "# Missing",
         y = "Variables") +
    theme_minimal() +
    theme(legend.position = "none")

}
