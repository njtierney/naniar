#' Plot of cumulative sum of missing value for each variable
#'
#' A plot showing the cumulative sum of missing values for each variable,
#' reading columns from the left to the right of the initial dataframe. A
#' default minimal theme is used, which can be customised as normal for ggplot.
#'
#' @param x a data.frame
#'
#' @return a ggplot object showing the cumulative sum of missings over the variables
#'
#' @seealso [geom_miss_point()] [gg_miss_case()] [gg_miss_case_cumsum()] [gg_miss_fct()] [gg_miss_span()] [gg_miss_var()] [gg_miss_which()]

#' @export
#'
#' @examples
#'
#' gg_miss_var_cumsum(airquality)

gg_miss_var_cumsum <- function(x) {
  ggobject <- x %>%
    miss_var_summary(add_cumsum = TRUE) %>%
    ggplot(aes(
      x = stats::reorder(variable, n_miss_cumsum),
      y = n_miss_cumsum,
      group = 1
    )) +
    geom_line(linewidth = 2) +
    labs(x = "Var", y = "Cumsum of missing values") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(ggobject)
}
