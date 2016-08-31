#' gg_missing_var
#'
#' This function draws a ggplot plot of the number of missings in each column, rearranged to show which variables have the most missing data.
#'
#' @param x a dataframe
#'
#' @return a ggplot plot depicting the number of missings in a given column
#' @export
#'
#' @examples
#'
#' gg_missing_var(airquality)
#'
gg_missing_var <- function(x){

  # get a tidy data frame of the number of missings in each column
  x %>%
    summary_missing_var() %>%
    ggplot(data = .,
           aes(x = reorder(variable, n_missing),
               y = n_missing,
               colour = variable)) +
    geom_bar(stat="identity", position="dodge", width = 0) +
    geom_point() +
    coord_flip() +
    labs(y = "# Missing",
         x = "Variables") +
    theme_minimal() +
    theme(legend.position = "none")

}
