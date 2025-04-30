#' Plot which variables contain a missing value
#'
#' This plot produces a set of rectangles indicating whether there is a missing
#'   element in a column or not.  A default minimal theme is used, which can be
#'   customised as normal for ggplot.
#'
#' @param x a dataframe
#'
#' @return a ggplot object of which variables contains missing values
#'
#' @seealso [geom_miss_point()] [gg_miss_case()] [gg_miss_case_cumsum()] [gg_miss_fct()] [gg_miss_span()] [gg_miss_var()] [gg_miss_var_cumsum()] [gg_miss_which()]
#'
#' @export
#'
#' @examples
#'
#' gg_miss_which(airquality)

gg_miss_which <- function(x) {
  col_na <- colSums(is.na(x)) == 0
  col_na_val <- dplyr::if_else(col_na, "complete", "missing")

  # tell us which columns have missing data
  ggobject <- tibble::tibble(variable = names(col_na), value = col_na_val) %>%
    dplyr::mutate(nheight = 1) %>%
    ggplot(data = ., aes(x = variable, y = nheight, fill = factor(value))) +
    geom_tile(colour = "white") +
    theme_minimal() +
    scale_fill_grey(name = "") +
    scale_x_discrete(limits = names(x)) +
    theme(legend.position = "none") +
    scale_y_discrete(breaks = c(""), labels = c("")) +
    labs(y = " ", x = " ") +
    coord_flip()

  return(ggobject)
}
