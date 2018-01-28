#' Plot of cumulative sum of missing for cases
#'
#' A plot showing the cumulative sum of missing values for cases, reading the
#' rows from the top to bottom. A default minimal theme is used, which can be
#' customised as normal for ggplot.
#'
#' @param x a dataframe
#' @param breaks the breaks for the x axis default is 20
#'
#' @return a ggplot object depicting the number of missings
#' @export
#'
#' @examples
#'
#' gg_miss_case_cumsum(airquality)
#' library(ggplot2)
#' gg_miss_case_cumsum(riskfactors, breaks = 50) + theme_bw()

gg_miss_case_cumsum <- function(x, breaks = 20){

  ggobject <- x %>%
    miss_case_cumsum() %>%
    ggplot(aes(x = stats::reorder(case, n_miss_cumsum),
               y = n_miss_cumsum,
               group = 1)) +
    geom_line(size = 2) +
    labs(x = "Case",
         y = "Cumulative sum of missing values") +
    scale_x_discrete(breaks = seq(0,
                                  nrow(x),
                                  by = breaks)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(ggobject)

}
