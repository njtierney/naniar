
#' Plot which variables contain a missing value
#'
#' This plot produces a set of rectangles indicating whether there is a missing
#'   element in a column or not.  A default minimal theme is used, which can be
#'   customised as normal for ggplot.
#'
#' @param x a dataframe
#'
#' @return a ggplot object
#'
#' @export
#'
#' @examples
#'
#' gg_miss_which(airquality)
#' library(ggplot2)
#'

gg_miss_which <- function(x){

  # tell us which columns have missing data
  ggobject <- x %>%
    purrr::map_df(anyNA) %>%
    purrr::map_df(function(x) ifelse(x == 0, "complete", "missing")) %>%
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
    scale_y_discrete(breaks = c(""),
                     labels = c("")) +
    labs(y = " ",
         x = " ")

  return(ggobject)

}
