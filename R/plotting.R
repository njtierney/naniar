# plotting functions

#' Plot the number of missings per case (row)
#'
#' This function draws a ggplot of the number of missings in each row.
#' At this point I'm not sure how this plot should be arranged, as it currently looks a bit ugly!
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

#' Plot the number of missings for each variable
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
           aes(x = stats::reorder(variable, n_missing),
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

#' Plot which variables contain a missing value
#'
#' \code{gg_missing_which} (need a better name!) produces a set of rectangles that indicate whether there is a missing element in a column or not
#'
#' @param x a dataframe
#'
#' @return a ggplot plot
#'
#' @export
#'
#' @examples
#'
#' gg_missing_which(airquality)
#'
gg_missing_which <- function(x){

  # tell us which columns have missing data
  airquality %>%
    purrr::dmap(anyNA) %>%
    purrr::dmap(function(x) ifelse(x == 0, "complete", "missing")) %>%
    tidyr::gather(key = "variable",
                  value = "value") %>%
    dplyr::mutate(nheight = 1) %>%
    ggplot(data = .,
           aes(x = variable,
               y = nheight,
               fill = factor(value))) +
    geom_tile(colour = "white") +
    theme_minimal() +
    scale_fill_grey(name = "") +
    scale_x_discrete(limits = names(x)) +
    theme(legend.position = "none") +
    scale_y_discrete(breaks=c(""),
                     labels=c("")) +
    labs(x = "",
         y = "")
}
