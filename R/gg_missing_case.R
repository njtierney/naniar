#' gg_missing_case
#'
#' This function draws a ggplot plot of the number of missings in each row.
#' At this point I'm not sure how this plot should be arranged, as it currently looks ugly!
#'
#' @param x a dataframe
#'
#' @return a ggplot plot depicting the number of missings in a given case
#' @export
#'
#' @examples
#'
#' gg_missing_case(airquality)
#'
gg_missing_case <- function(x){
# x <- airquality

  ggplot(data = summary_missing_case(x),
         aes(x = n_missing,
             y = case)) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    labs(x = "# Missing",
         y = "Case") +
    theme_minimal()

}
