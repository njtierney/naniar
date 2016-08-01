#' gg_missing_which
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
    purrr::dmap(any_na) %>%
    purrr::dmap(function(x) ifelse(x == 0, "complete", "missing")) %>%
    tidyr::gather(key = "variable",
                  value = "value") %>%
    mutate(nheight = 1) %>%
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
