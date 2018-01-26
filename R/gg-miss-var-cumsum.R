#' Plot of cumulative sum of missing value for each variable
#'
#' A plot showing the cumulative sum of missing values for each variable,
#' reading columns from the left to the right of the initial dataframe. A
#' default minimal theme is used, which can be customised as normal for ggplot.
#'
#' @param x a data.frame
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' gg_miss_var_cumsum(airquality)

gg_miss_var_cumsum <- function(x){

  ggobject <- x %>%
    miss_var_cumsum() %>%
    ggplot(aes(x = stats::reorder(variable, n_miss_cumsum),
               y = n_miss_cumsum,
               group = 1)) +
    geom_line(size = 2) +
    labs(x = "Var",
         y = "Cumsum of missing values") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(ggobject)
}
