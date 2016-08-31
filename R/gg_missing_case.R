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

  ggplot(data = summary_missing_case(x),
         aes(y = n_missing,
             x = case)) +
    geom_bar(stat="identity", position="dodge", width = 0, colour="grey") +
    geom_point() +
    coord_flip() +
    labs(y = "# Missing",
         x = "Cases") +
    theme_minimal()

}
